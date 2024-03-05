/* This file is part of GTI (Generic Tool Infrastructure)
 *
 * Copyright (C)
 *  2008-2019 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2013-2019 RWTH Aachen University, Federal Republic of Germany
 *
 * See the LICENSE file in the package base directory for details
 */

/**
 * @file ModuleBase.h
 *       @see gti::ModuleBase
 *
 * @author Tobias Hilbrich
 *
 * Complete overhaul done 20.10.2010.
 *
 */

#include <sys/types.h>
#include <mpi.h>
#include <pnmpimod.h>
#include <assert.h>
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <map>
#include <list>
#include <vector>

#include "GtiTLS.h"

#include "GtiDefines.h"
#include "GtiEnums.h"
#include "GtiTypes.h"

#include "I_Module.h"
#include "I_Place.h"
#include <functional>

#if defined(GTI_THREAD_SAFETY) && !defined(USE_THREAD_LOCAL)
#include "safe_ptr.h"
#endif

#ifndef MODULE_BASE_H
#define MODULE_BASE_H

#include <memory>

//#define USE_THREAD_LOCAL

namespace gti
{
/**
 * Base class for common module behavior.
 * Includes a common initialization and destruction
 * mechanism for sub modules. As well as parsing and
 * access to module data. It uses two templates:
 * - "T" type of the final object to create; used when
 *     allocating an instance in ModuleBase::getInstance
 * - "Base" base class from which ModuleBase should
 *   inherit, e.g. I_Module. If it is a different class
 *   than I_Module, "Base" must inherit
 *   from I_Module itself or indirectly.
 *
 * See @ref ModConfPage for details.
 *
 * ModuleBase is used as follows:
 * - In PNMPI_RegistrationPoint the static member ModuleBase::readModuleInstances
 *   must be called in order to read all named instances
 *   from the PnMPI configuration. Needs to register the module with the module
 *   name given in the PnMPI configuration file. Needs to register the freeInstance
 *   and getInstance functions see below.
 * - getInstance must call ModuleBase::getInstance and pass the given instance name
 *   to it.
 * - freeInstance must call ModuleBase::freeInstance and pass the given instance
 *   pointer to it.
 * - A class using ModuleBase needs to require an instance name as an argument to
 *   its constructor.
 * - This instance name must be passed to the constructor of ModuleBase
 * - The class using ModuleBase may call:
 * -- ModuleBase::createsSubModuleInstance to initialize sub modules specified for
 *       this instance in the PnMPI configuration file. A list with pointers to the
 *       initialized sub modules is returned.
 * -- ModuleBase::destroySubModule to destroy a sub module instance.
 * -- ModuleBase::getData to receive a list of key value pairs given as data to this
 *       instance.
 *
 * Furthermore, ModuleBase provides a default implementation for the functions of
 * gti::I_Module. Defaulting to a not traceable object.
 *
 * Example:
 * @code
namespace gti
{
        class MyInstance : public ModuleBase<MyInstance, I_Module>
        {
        protected:
                std::vector<I_Module*> mySubMods;

        public:
                MyInstance (const char* InstanceName);
                ~MyInstance (void);
        };
}//namespace gti

using namespace gti;

MyInstance::MyInstance (const char* InstanceName)
        : ModuleBase<MyInstance, I_Module> (InstanceName)
{
        std::cout << "Constructing instance." << std::endl;

        mySubMods = createSubModuleInstances ();
}

MyInstance::~MyInstance (void)
{
        std::cout << "Destructing instance." << std::endl;

        for (int i = 0; i < mySubMods.size(); i++)
        {
                destroySubModuleInstance (mySubMods[i]);
        }
}
         @endcode
 *
 * Use the macros in GtiMacros.h to implement getInstance, freeInstance and
 * PnMPI_RegistrationPoint.
 *
 */

#ifdef GTI_THREAD_SAFETY

/**
 * The TLS Wrapper class transparently provides thread-local storage by
 * either using thread_local variables or managing a vector with entries
 * for each thread.
 * The threadlocal values are lazy initialized to a selected default
 * value passed to the class constructor, or to the default initialized
 * value of the base class.
 * The class constructor also accepts an initializer function to be
 * called once for each thread, when after the value was initilized.
 */
template <class T>
class TLSWrapper
{
    T _defaultValue{};
    std::function<void(void)> _initializer{nullptr};
    void init()
    {
        if (_initializer)
            _initializer();
    }
#if defined(USE_THREAD_LOCAL)
    thread_local T _data{};
    thread_local bool _inited{false};

  public:
    /** get a pointer to the thread-local instance */
    T* getData()
    {
        if (!_inited) {
            _inited = true;
            _data = _defaultValue;
            init();
        }
        return &_data;
    }
#else
    sf::contfree_safe_ptr<std::vector<std::unique_ptr<T>>> _data{};
    sf::contfree_safe_ptr<std::vector<bool>> _inited{};

  public:
    /** get a pointer to the thread-local instance */
    T* getData()
    {
        typename decltype(_inited)::obj_t::size_type tid = getGtiTid();
        auto _inited_size = slock_safe_ptr(_inited)->size();
        bool needs_init = false;
        if (_inited_size < tid + 1) {
            needs_init = true;
            auto xsafe_inited = xlock_safe_ptr(_inited);
            if (xsafe_inited->size() < tid + 1)
                xsafe_inited->resize(tid + 1, false);
            xsafe_inited->at(tid) = true;
        } else if (!slock_safe_ptr(_inited)->at(tid)) {
            needs_init = true;
            xlock_safe_ptr(_inited)->at(tid) = true;
        }
        if (needs_init) {
            T* ret;
            {
                auto xsafe_data = xlock_safe_ptr(_data);
                if (xsafe_data->size() < tid + 1) {
                    xsafe_data->resize(tid + 1);
                }
                ret = new T{_defaultValue};
                (*xsafe_data)[tid].reset(ret);
            }
            init();
            return ret;
        }
        return slock_safe_ptr(_data)->at(tid).get();
    }
#endif
    /** get the value of the thread-local instance */
    T getValue() { return *getData(); }
    /** set the value of the thread-local instance */
    void setValue(T& value) { *getData() = value; }
    /** set the value of the thread-local instance */
    void setValue(T value) { *getData() = value; }
    /** constructor with default value */
    TLSWrapper(T defaultValue) : _defaultValue(defaultValue) {}
    /** constructor with default value and initializer function */
    TLSWrapper(T defaultValue, std::function<void(void)> initializer)
        : _defaultValue(defaultValue), _initializer(initializer)
    {
    }
    /** constructor with initializer function */
    TLSWrapper(std::function<void(void)> initializer) : _initializer(initializer) {}
    /** default constructor */
    TLSWrapper() {}
};
#endif

template <class T, class Base, bool TLS = true>
class ModuleBase : public Base
{
  private:
    class wrapMap
    {
      private:
        std::map<std::string, std::pair<T*, int>> Map;

      public:
        std::map<std::string, std::pair<T*, int>>& getMap() { return Map; }
        ~wrapMap()
        {
            for (typename std::map<std::string, std::pair<T*, int>>::iterator iter = Map.begin();
                 iter != Map.end();
                 iter++) {
                if (iter->second.first) {
                    //          iter->second.first->myRefCount--;
                    //          printf("%s: %i instances left\n", __PRETTY_FUNCTION__,
                    //          iter->second.first->myRefCount);
                    if (iter->second.first->myRefCount == 0)
                        delete iter->second.first;
                    //          else
                    //            printf("%s: %i instances left\n", __PRETTY_FUNCTION__,
                    //            iter->second.first->myRefCount);
                }
            }
            Map.clear();
        }
    };
    /*
     * Typedefs.
     */
    typedef std::map<std::string, std::string> DataMapT; /**< Type for the data map list. */
    typedef std::map<std::string, std::pair<T*, int>>
        ModInstancesT; /**< Type for mapping of known and used instances and their indices. */
    typedef std::list<std::pair<std::string, std::string>>
        SubModNamesT; /**< Type for list of sub module identifiers (pairs of ModName and
                         InstanceName). */

    /*
     * Static attributes.
     * Keep in mind these two attributes need to be distinct for each type of module,
     * as the first template parameter is going to differ for each type of module this
     * will hold even though one might think otherwise at first.
     */

    /* Maps instance name to instance pointers and instance index, also used to store the names of
     * defined instances.*/

#ifdef GTI_THREAD_SAFETY /* Without thread safety we do not differ between TLS or not */
    /* First version keeps a map per thread. So every thread has a own instance of this module */
    template <
        class t = T,
        class base = Base,
        bool tls = TLS,
        typename std::enable_if<tls>::type* = nullptr>
    static ModInstancesT& ourInstances()
    {
        static TLSWrapper<wrapMap> instances{[&]() { readModuleInstances(ourModHandle()); }};
        ModInstancesT& i = instances.getData()->getMap();
        return i;
    }

    /* Second version keeps a map per process. So every thread has an own instance of this module */
    template <
        class t = T,
        class base = Base,
        bool tls = TLS,
        typename std::enable_if<!tls>::type* = nullptr>
#endif /* Also, without GTI_THREAD_SAFETY we might not have a c++11 compiler, so hide the          \
          enable_if */
    static ModInstancesT& ourInstances()
    {
        static wrapMap instances;
        static bool inited = false;
        ModInstancesT& i = instances.getMap();
        if (!inited) {
            inited = true;
            readModuleInstances(ourModHandle());
        }
        return i;
    }

    //=============================
    // Static attribute construction: Accessor for ourDataFromAncestors
    //=============================
#ifdef GTI_THREAD_SAFETY
    static std::mutex ancestorDataMutex;
    template <
        class t = T,
        class base = Base,
        bool tls = TLS,
        typename std::enable_if<tls>::type* = nullptr>
    static std::map<std::string, typename ModuleBase<T, Base, TLS>::DataMapT>&
    ourDataFromAncestors()
    {
        static TLSWrapper<std::map<std::string, typename ModuleBase<T, Base, TLS>::DataMapT>>
            dataFromAncestors{[&]() { readModuleInstances(ourModHandle()); }};
        return *dataFromAncestors.getData();
    }
    template <
        class t = T,
        class base = Base,
        bool tls = TLS,
        typename std::enable_if<!tls>::type* = nullptr>
#endif
    static std::map<std::string, typename ModuleBase<T, Base, TLS>::DataMapT>&
    ourDataFromAncestors()
    {

        static std::map<std::string, typename ModuleBase<T, Base, TLS>::DataMapT> dataFromAncestors;
        static bool inited = false;
        if (!inited) {
            inited = true;
            readModuleInstances(ourModHandle());
        }
        return dataFromAncestors;
    }

    // TODO: should this be handled like ourInstances ?
    static PNMPI_modHandle_t&
    ourModHandle(); /**< The module handle for this module, set during readModuleInstances. */
    static std::string& ourModName(); /**< Name of this module. */
    //	static std::map<std::string, DataMapT >& ourDataFromAncestors(); /** Data passed from
    // ancestor modules to instances of this module. */

    PNMPI_modHandle_t getWrapperHandle();
    PNMPI_Service_descriptor_t getWrapperService(const char* serviceName, const char* serviceSig);

    /*
     * Regular attributes.
     */
    int myRefCount;             /**< Reference count for each instance of a module.*/
    SubModNamesT mySubModNames; /**< (ModuleName, InstanceName) pairs for all sub modules, first
                                   element is first sub-module.*/
    DataMapT myModData;         /**< Key value pairs that describe the module data.*/
    std::string myInstanceName; /**< Name of this instance.*/

    std::vector<int>
        myLevelSizes; /**< Sizes of this and its ancestor tool levels, only set on demand.*/
    std::vector<GTI_DISTRIBUTION>
        myDistributions; /**< Distributions between the levels; [0] is the distribution between
                            ancestor of index 0 and ancestor of index 1.*/
    std::vector<int>
        myBlocksizes; /**< Blocksize for blocksize distributions, see myDistributions.*/
    int myOwnLevelId; /**< Own level index, only set on demand, base value is -1.*/
    getFunction_t myGetFunctionWrapperService = nullptr;
    //  I_Place * placeMod;
    I_Place* myGetPlaceMod();

  protected:
    /**
     * Constructor.
     * @param instanceName name of this instance.
     */
    ModuleBase(const char* instanceName);

    /**
     * Empty virtual destructor.
     */
    virtual ~ModuleBase(){};

    /**
     * Interprets the sub modules given in the PnMPI
     * configuration file for this instance and creates
     * all of these instances. Returns the pointer
     * to their instances as a vector.
     * @return vector of created instances.
     */
    std::vector<I_Module*> createSubModuleInstances(void);

    /**
     * Frees the given instance of a sub module.
     * The sub-module must have been created with
     * createSubModuleInstances.
     * @param instance pointer to instance that shall be freed.
     * @return GTI_SUCCESS if successfull.
     */
    GTI_RETURN destroySubModuleInstance(I_Module* instance);

#if 0	
/**
	 * Use the registered module name to get a module instance
	 * @return an instance.
	 */
	I_Module* getInstanceByName (std::string name);
#endif

    /**
     * Adds a specified key value pair to the module data
     * of all sub modules and their respective sub modules
     * (recursive).
     * This data will only be considered if the respective
     * sub modules where not yet created.
     * @param key name of the data key.
     * @param value for data key.
     * @return GTI_SUCCESS if successful, GTI_ERROR otherwise.
     */
    GTI_RETURN addDataToSubmodules(std::string key, std::string value);

    /**
     * Returns the address to a function in a wrapper module.
     * The function is only available if the given function is describes in an API specification,
 * if it is marked as a wrap-everywhere function, and if some module in this or any
 * descendant layer is interested in some argument of the function.
     * @param functionName name of the function to be found in the wrapper.
     * @param pOutFunction pointer to storage for a function pointer
     *        only set if the call is successful.
     * @return GTI_SUCCESS if the function was found, GTI_ERROR otherwise.
     *
     * Example:
     * @code
            int (*newSize) (int);
            if (getWrapperFunction ("newSize", (GTI_Fct_t*)&newSize) == GTI_SUCCESS)
            {
                    for (int i = 0; i < c; i++)
                            (*newSize) (array[i]);
            }
            else
            {
                    std::cout << "ERROR: failed to get \"newSize\" function pointer from wrapper."
 << std::endl;
            }
     @endcode
     */
    GTI_RETURN getWrapperFunction(std::string functionName, GTI_Fct_t* pOutFunction);

    /**
     * Returns the address to a wrap-across function. This is a function whose arguments
     * can be transfered to a different place within the same tool layer.
     * The function is only available if the given function is describes in an API specification,
     * if it is marked as a wrap-across function, and if some module is on this layer is interested
     * in some argument of the function.
     * @param functionName name of the function to be found in the wrapper.
     * @param pOutFunction pointer to storage for a function pointer
     *        only set if the call is successful.
     * @return GTI_SUCCESS if the function was found, GTI_ERROR otherwise.
     *
     * Example:
     * @code
            int (*pPublishSend) (int tag, int toChannel);
            if (getWrapAcrossFunction ("publishSend", (GTI_Fct_t*)&pPublishSend) == GTI_SUCCESS)
            {
                    (*pPublishSend) (tag,toChannel);
            }
            else
            {
                std::cout << "ERROR: failed to get \"publishSend\" function pointer as wrap-across
     function." << std::endl;
            }
         @endcode
     */
    GTI_RETURN getWrapAcrossFunction(std::string functionName, GTI_Fct_t* pOutFunction);

    /**
     * Returns the address to a broadcast function. This is a function whose arguments
     * can be transfered to all ancestor levels.
     * The function is only available if the given function is describes in an API specification,
     * if it is marked as a wrap-down function, and if some module on any ancestor layer is
     interested
     * in some argument of the function.
     * @param functionName name of the function to be found in the wrapper.
     * @param pOutFunction pointer to storage for a function pointer
     *        only set if the call is successful.
     * @return GTI_SUCCESS if the function was found, GTI_ERROR otherwise.
     *
     * Example:
     * @code
                int (*pPublishSend) (int tag, int toChannel);
                if (getBroadcastFunction ("notifyOfImpedingDeath",
     (GTI_Fct_t*)&pLetThemKnowTheyWillDie) == GTI_SUCCESS)
                {
                        (*pLetThemKnowTheyWillDie) (errorReason);
                }
                else
                {
                    std::cout << "ERROR: failed to get \"notifyOfImpedingDeath\" function pointer as
     broadcast function." << std::endl;
                }
             @endcode
     */
    GTI_RETURN getBroadcastFunction(std::string functionName, GTI_Fct_t* pOutFunction);

    /**
     * Returns the address of the function that notifies the wrapper module that the next injected
     * event uses a special strided channel id.
     * @param pOutFunction pointer to storage for a function pointer
     *        the function is of type gtiSetNextEventStridedP from GtiApi.h.
     * @return GTI_SUCCESS if the function was found, GTI_ERROR otherwise.
     *
     */
    GTI_RETURN getSetNextEventStridedFunction(GTI_Fct_t* pOutFunction);

    /**
     * Assuming application processes are ranked from 0-(N-1),
     * for N tasks, this function provides information on which
     * process on this tool level will receive information from a
     * particular rank.
     * Critical is the specification of "rank", this is the piece of
     * information that is used to distributed the application
     * tasks to the processes on the first tool level. It may be
     * dependent on the application and communication type.
     *
     * @todo This must be refined in more detail in the future,
     *              for MPI this is the MPI rank of a process.
     *
     * Usage is intended for intra layer communication to determine
     * the right communication target.
     *
     * @param rank for which to find the process that receives
     *                  information of the rank on this tool level.
     * @param outLevelId pointer to storage for an int that will
     *                  hold the id of the level that receives the information.
     * @param GTI_SUCCESS if the input rank is valid and
     *                  no error occurred.
     */
    GTI_RETURN getLevelIdForApplicationRank(int rank, int* outLevelId);

    /**
     * Returns the rank interval that sends records to this TBON node.
     * Uses the same assumptions as getLevelIdForApplicationRank.
     *
     * @see getLevelIdForApplicationRank
     *
     * @param outBegin beginning of interval.
     * @param outLevelId end of interval.
     * @param rank some rank that sends events to this node (used to identify "this" node)
     * @param GTI_SUCCESS if no error occurred.
     */
    GTI_RETURN getReachableRanks(int* outBegin, int* outEnd, int rank);

    /**
     * Returns the rank interval that sends records to this TBON node.
     * Uses the same assumptions as getLevelIdForApplicationRank.
     *
     * @see getLevelIdForApplicationRank
     * @see getReachableRanks
     *
     * @param outBegin beginning of interval.
     * @param outLevelId end of interval.
     * @param ownId the level-specific id of "this" node, i.e., the rank within the comm of the
     * layer
     * @param GTI_SUCCESS if no error occurred.
     */
    GTI_RETURN getReachableRanksForOwnId(int* outBegin, int* outEnd, int ownId);

    /**
     * Returns the id of this level, where 0 is application layer, 1 its parent layer and so forth.
     *
     * @param outThisLevelId pointer to storage for integer that is set to this level id.
     * @param GTI_SUCCESS if no error occurred.
     */
    GTI_RETURN getLevelId(int* outThisLevelId);

    /**
     * Provides number of input channels.
     * @param numChannels output.
     * @param GTI_SUCCESS if no error occurred.
     */
    GTI_RETURN getNumInputChannels(int* numChannels);

    /**
     * Provides the pointer to the Placement driver module.
     * @param placeMod output.
     * @param GTI_SUCCESS if no error occurred.
     */
    GTI_RETURN getPlaceMod(I_Place** placeMod);

    /**
     * Provides the id of the node in the current layer.
     * @param id output.
     * @param GTI_SUCCESS if no error occurred.
     */
    GTI_RETURN getNodeInLayerId(GtiTbonNodeInLayerId* id);

  public:
    bool usesTLS() { return TLS; }

    /**
     * Returns the instance with the given name.
     * Constructs it if it is queried for the
     * first time.
     *
     * SHOULD ONLY BE CALLED BY THE MACROS PROVIDED
     * IN GtiMacros.h !
     *
     * @param instanceName name of the instance.
     *        If an empty string the instance
     *        with index 0 will be returned
     *        (assumed there is an instance).
     * @return the instance or NULL if an unknown
     *         instance name was used or another
     *         error occured.
     */
    static T* getInstance(std::string instanceName);

    /**
     * Frees the given instance.
     * Destructs it when the last user of the instance
     * calls this function.
     *
     * SHOULD ONLY BE CALLED BY THE MACROS PROVIDED
     * IN GtiMacros.h !
     *
     * @param instance the instance to be freed.
     * @return GTI_SUCCESS if successful.
     */
    static GTI_RETURN freeInstance(T* instance);

    /**
     * Frees the instance even if its reference
     * count indicates that there are still other
     * users of the instance.
     *
     * SHOULD ONLY BE CALLED BY THE MACROS PROVIDED
     * IN GtiMacros.h !
     *
     * @param instance the instance to be freed.
     * @return GTI_SUCCESS if successful.
     */
    static GTI_RETURN freeInstanceForced(T* instance);

    /**
     * Reads the names of all instances used for this module.
     * Must be called when PNMPI_RegistrationPoint is entered.
     * If it is not called no instances can be retrieved from
     * getInstance and freeInstance.
     *
     * SHOULD ONLY BE CALLED BY THE MACROS PROVIDED
     * IN GtiMacros.h !
     *
     * @param modHandle handle to this module.
     * @return GTI_SUCCESS if successful.
     */
    static GTI_RETURN readModuleInstances(PNMPI_modHandle_t modHandle);
    static void initModuleOnce(PNMPI_modHandle_t modHandle);
    /**
     * Adds data to a module instance.
     * The data will only be considered if the named instance exists and
     * was not yet constructed. If the data uses the same key as a
     * previously added data item or a data item in the P^nMPI configuration
     * than the last call to addData will set the value for this key.
     * @param instanceName name of the module instance to add the data
     *        to.
     * @param key name of key used for the data.
     * @param value of the data.
     * @return GTI_SUCCESS if successful, GTI_ERROR otherwise.
     */
    static GTI_RETURN addData(std::string instanceName, std::string key, std::string value);

    /**
     * Returns a list of all currently active instances.
     * Active refers to allocated and initialized while not
     * yet destroyed.
     */
    static std::map<std::string, T*> getActiveInstances(void);

    /**
     * Returns the name of this module.
     * Not the instance name!
     * @return name of this module.
     */
    std::string getName(void);

    /**
     * Returns the data associated with this module.
     * The data is returned as a map, were key is the
     * data attribute name and value the data value.
     * E.g.
     * @code
                       ModuleBase<MyModule, I_Module> mod (INSTANCE_NAME);
                       std::map<std::string,std::string> map;
                       map = mod.getData ();
                       //check for the "ip" data attribute
                       map::iterator i = map.find ("ip");
                       if (i != map::end())
                       {
                           printf ("My module IP is: %s\n", (i->second).c_str());
                       }
                       else
                       {
                           printf ("No IP specified for this module.\n");
                       }
               @endcode
     *
     * @return map for the module data attributes.
     */
    DataMapT getData(void);

}; /*class ModuleBase*/
} /*namespace gti*/

/*Include implementation*/
#include "ModuleBase.hxx"

#endif /* MODULE_BASE_H */
