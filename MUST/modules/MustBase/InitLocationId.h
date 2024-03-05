/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file InitLocationId.h
 *       @see must::InitLocationId.
 *
 *  @date 24.04.2014
 *  @author Tobias Hilbrich
 */

#include "ModuleBase.h"
#include "I_InitLocationId.h"
#include "mustConfig.h"
#include "LocationInfo.h"
#include "I_InitParallelId.h"
#include "I_GenerateLocationId.h"
#include "BaseApi.h"

#if defined(BUILD_BACKWARD)
#include "backward.hpp"
#endif

#ifndef INITLOCATIONID_H
#define INITLOCATIONID_H

using namespace gti;

namespace must
{
/**
 * Implementation to set the location ID with either just a callname or with a callname and a call
 * stack.
 */
class InitLocationId : public gti::ModuleBase<InitLocationId, I_InitLocationId>
{
  protected:
    /**
     * Maps location id to its information.
     * Two versions depending of whether we have stack information or not.
     */
#ifndef ENABLE_STACKTRACE
    /** Maps locationID (which is just a GTI call id in that case) to both a information that
     * describes this location and an occurrence count*/
    typedef std::map<MustLocationId, std::pair<LocationInfo, uint32_t>> KnownLocationsType;
#else
#if defined(BUILD_BACKWARD)
    /** Maps a GTI call id to a map of call stacks known for that call id and
     * an occurrence count*/
    typedef std::map<int, std::pair<std::map<LocationInfoImpl<uint64_t>, MustLocationId>, uint32_t>>
        KnownLocationsType;
#else
    /** Maps a GTI call id to a map of call stacks known for that call id and
     * an occurrence count*/
    typedef std::map<int, std::pair<std::map<LocationInfo, MustLocationId>, uint32_t>>
        KnownLocationsType;
#endif
#endif
#if defined(BUILD_BACKWARD)
    static thread_local backward::TraceResolver tr;
#endif

    KnownLocationsType myKnownLocations;

    I_InitParallelId* myPIdInit;
    I_GenerateLocationId* myGenLId;
    handleNewLocationP myNewLocFct;

#ifdef ENABLE_STACKTRACE
    /**
     * Helper function to call the handleNewLocation call when we use the callpath module
     * @param id of the new location
     * @param callName of the call
     * @param location stack information
     */
    void
    createHandleNewLocationCall(MustLocationId id, const char* callName, LocationInfo& location);
#endif

    MustLocationId uniqueLocationId;

  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    InitLocationId(const char* instanceName);

    /**
     * Destructor.
     */
    ~InitLocationId() override;

    /**
     * @see I_InitLocationId::init
     */
    GTI_ANALYSIS_RETURN init(MustLocationId* pStorage, const char* callName, int callId) override;

    GTI_ANALYSIS_RETURN getUniqueLocationId(MustLocationId* pStorage);

    /**
     * @copydoc I_InitLocationId::initCodePtr
     */
    gti::GTI_ANALYSIS_RETURN initCodePtr(
        const char* callname,
        const void* callptr,
        MustLocationId* pStorage,
        const void* codeptr_ra) override;

  private:
    size_t vmaOffset; // required for addr2line translation
};                    /*class InitLocationId */
} // namespace must

#endif /*INITLOCATIONID_H*/
