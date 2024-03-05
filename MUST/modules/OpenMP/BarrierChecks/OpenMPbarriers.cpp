/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "OpenMPbarriers.h"

#include "GtiMacros.h"
#include "MustEnums.h"

using namespace gti;
using namespace must;

mGET_INSTANCE_FUNCTION(OpenMPbarriers);
mFREE_INSTANCE_FUNCTION(OpenMPbarriers);
mPNMPI_REGISTRATIONPOINT_FUNCTION(OpenMPbarriers);

/*
 * Initialization if static member variables
 */
std::mutex OpenMPbarriers::active_barriers_mut{};
std::map<OpenMPbarriers::Region, OpenMPbarriers::Barrier> OpenMPbarriers::active_barriers{};

/* Constructor.
 *
 * For a detailed documentation see the related header file.
 */
OpenMPbarriers::OpenMPbarriers(const char* instanceName)
    : ModuleBase<OpenMPbarriers, I_OpenMPbarriers>(instanceName)
{
    /* Get submodules.
     *
     * A pointer to the dependent modules will be stored in member variables,
     * making them accessible for the lifetime of this object. */
    std::vector<I_Module*> subModInstances = createSubModuleInstances();
    logger = static_cast<I_CreateMessage*>(subModInstances[0]);
}

/* Notify the module about the begin of a barrier.
 *
 * For a detailed documentation see the related header file.
 */
GTI_ANALYSIS_RETURN OpenMPbarriers::notifyBarrierBegin(
    MustParallelId pId,
    MustLocationId lId,
    uint64_t parallel_data,
    MustAddressType codeptr_ra)
{
    const auto& parallel_region = parallel_data;
    const auto& barrier = codeptr_ra;
    if (parallel_region == 0 || barrier == 0) {
        // Ignore implicit barriers at end of parallel region
        return gti::GTI_ANALYSIS_SUCCESS;
    }
    std::lock_guard<std::mutex> lock{active_barriers_mut};

    /* Search for an active barrier of the thread's parallel team. If a team can
     * be found, this thread will join the it. */
    auto it = active_barriers.find(parallel_region);
    if (it == active_barriers.end()) {
        /* This thread is the first one of its team to hit the barrier. A new team
         * will be registered for this barrier in the internal map. */
        active_barriers.insert(std::make_pair(parallel_region, barrier));
    } else if (it->second != barrier) {
        /* If the new thread hits a different barrier than the other members of
         * the team, an error message will be generated, as all threads of a
         * team need to pass the same barrier, or a deadlock occurs.
         *
         * NOTE: The barrier will be identified by its location ID, as it is
         *       unique for the source code location of a call and thus all
         *       calls for the same barrier have the same location ID.
         *
         * NOTE: The final barrier of a parallel region has a location ID of
         *       zero. As this event is dispatched only once per parallel region
         *       and has no meaning for this analysis, it will be ignored. */
        logger->createMessage(
            MUST_ERROR_OPENMP,
            pId,
            lId,
            MustErrorMessage,
            "Thread passes a different barrier than "
            "other threads of the same team.");
        return GTI_ANALYSIS_FAILURE;
    }

    return GTI_ANALYSIS_SUCCESS;
}

/* Notify the module about the end of a barrier.
 *
 * For a detailed documentation see the related header file.
 */
GTI_ANALYSIS_RETURN OpenMPbarriers::notifyBarrierEnd(
    MustParallelId pId,
    MustLocationId lId,
    uint64_t parallel_data,
    MustAddressType codeptr_ra)
{
    const auto& parallel_region = parallel_data;
    const auto& barrier = codeptr_ra;
    if (parallel_region == 0 || barrier == 0) {
        // Ignore implicit barriers at end of parallel region
        return gti::GTI_ANALYSIS_SUCCESS;
    }
    std::lock_guard<std::mutex> lock{active_barriers_mut};
    /* Search for the active barrier of the thread and remove it from the map to
     * mark the barrier as passed.
     *
     * NOTE: Notifications for barriers not begun before will be ignored, as
     *       this event should never occur and if it does, we can't issue any
     *       error message related to the type of check this analysis does. */
    auto it = active_barriers.find(parallel_region);
    if (it != active_barriers.end()) {
        active_barriers.erase(it);
    }

    return GTI_ANALYSIS_SUCCESS;
}

OpenMPbarriers::~OpenMPbarriers()
{
    if (logger != nullptr) {
        destroySubModuleInstance(logger);
    }
}
