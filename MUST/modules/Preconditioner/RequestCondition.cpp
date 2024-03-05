/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file RequestCondition.cpp
 *       @see MUST::RequestCondition.
 *
 *  @date 06.06.2011
 *  @author Joachim Protze
 */

#include "GtiMacros.h"
#include "MustEnums.h"
#include "PrefixedOstream.hpp"

#include "RequestConditionApi.h"

#include "RequestCondition.h"

using namespace must;

mGET_INSTANCE_FUNCTION(RequestCondition)
mFREE_INSTANCE_FUNCTION(RequestCondition)
mPNMPI_REGISTRATIONPOINT_FUNCTION(RequestCondition)

//=============================
// Constructor
//=============================
RequestCondition::RequestCondition(const char* instanceName)
    : gti::ModuleBase<RequestCondition, I_RequestCondition>(instanceName), myAnySource(-1),
      myCallbacks()
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUB_MODS 0
#if NUM_SUB_MODS > 0
    if (subModInstances.size() < NUM_SUB_MODS) {
        must::cerr << "Module has not enough sub modules, check its analysis specification! ("
                   << __FILE__ << "@" << __LINE__ << ")" << std::endl;
        assert(0);
    }
#endif
    if (subModInstances.size() > NUM_SUB_MODS) {
        for (std::vector<I_Module*>::size_type i = NUM_SUB_MODS; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    // Initialize module data
    getWrapperFunction("propagateRequestRealComplete", (GTI_Fct_t*)&myPComplete);
    getWrapperFunction("propagateRequestsRealComplete", (GTI_Fct_t*)&myPCompletes);
    getWrapperFunction("executeCommDup", (GTI_Fct_t*)&myPCommDup);
}

//=============================
// Destructor
//=============================
RequestCondition::~RequestCondition() {}

//=============================
// addPredefineds
//=============================
GTI_ANALYSIS_RETURN RequestCondition::addPredefineds(int anySource)
{
    myAnySource = anySource;
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// complete
//=============================
GTI_ANALYSIS_RETURN RequestCondition::complete(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType request,
    int flag,
    int statusSource)
{
    // Is the flag true ?
    if (!flag)
        return GTI_ANALYSIS_SUCCESS;

    auto x_safe_Callbacks = xlock_safe_ptr(myCallbacks);
    auto callback_it = x_safe_Callbacks->find(request);
    if (callback_it != x_safe_Callbacks->end()) {
        callback_it->second();
        myCallbacks->erase(callback_it);
    }

    if (myPComplete) {
        (*myPComplete)(pId, lId, statusSource, request);
    }

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// completeAny
//=============================
GTI_ANALYSIS_RETURN RequestCondition::completeAny(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType* requests,
    int count,
    int index,
    int flag,
    int statusSource)
{
    if (!flag) {
        return GTI_ANALYSIS_SUCCESS;
    }
    if (index >= count || index < 0) {
        return GTI_ANALYSIS_SUCCESS;
    }

    return complete(pId, lId, requests[index], true, statusSource);
}

//=============================
// completeArray
//=============================
GTI_ANALYSIS_RETURN RequestCondition::completeArray(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType* requests,
    int count,
    int flag,
    int* statusSources)
{
    if (!flag || count <= 0)
        return GTI_ANALYSIS_SUCCESS;

    if (myPCompletes) {
        (*myPCompletes)(pId, lId, statusSources, requests, count);
    }

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// completeSome
//=============================
GTI_ANALYSIS_RETURN RequestCondition::completeSome(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType* requests,
    int count,
    int* indices,
    int numIndices,
    int* statusSources)
{
    if (numIndices <= 0 || count <= 0)
        return GTI_ANALYSIS_SUCCESS;
    MustRequestType* completedRequests = new MustRequestType[numIndices];
    for (int i = 0; i < numIndices; i++) {
        completedRequests[i] = requests[indices[i]];
    }
    GTI_ANALYSIS_RETURN retval =
        completeArray(pId, lId, completedRequests, numIndices, true, statusSources);
    delete[] completedRequests;
    return retval;
}

GTI_ANALYSIS_RETURN RequestCondition::commIdup(
    MustParallelId pId,
    MustLocationId lId,
    MustCommType comm,
    void* newcomm,
    MustRequestType request)
{
    auto x_safe_Callbacks = xlock_safe_ptr(myCallbacks);
    x_safe_Callbacks->emplace(request, [=]() {
        if (myPCommDup) {
            (*myPCommDup)(pId, lId, comm, (MustCommType) * (MPI_Comm*)newcomm);
        }
        return GTI_ANALYSIS_SUCCESS;
    });
    return GTI_ANALYSIS_SUCCESS;
}

/*EOF*/
