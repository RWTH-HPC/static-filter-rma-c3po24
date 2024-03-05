/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "OpenMPlocks.h"

#include "BaseApi.h"
#include "GtiMacros.h"
#include "MustEnums.h"
#include "ScopeGuard.hpp"

using namespace gti;
using namespace must;

mGET_INSTANCE_FUNCTION(OpenMPlocks);
mFREE_INSTANCE_FUNCTION(OpenMPlocks);
mPNMPI_REGISTRATIONPOINT_FUNCTION(OpenMPlocks);

openmp::LockExtensions OpenMPlocks::myLockExtensions{};
openmp::LockTracker OpenMPlocks::myTracker{};

/* Constructor.
 *
 * For a detailed documentation see the related header file.
 */
OpenMPlocks::OpenMPlocks(const char* instanceName)
    : ModuleBase<OpenMPlocks, I_OpenMPlocks>(instanceName)
{
    /* Get submodules.
     *
     * A pointer to the dependent modules will be stored in member variables,
     * making them accessible for the lifetime of this object. */
    std::vector<I_Module*> subModInstances = createSubModuleInstances();
    myLogger = static_cast<I_CreateMessage*>(subModInstances[0]);
}

/* Notify the module about a lock being initialized.
 *
 * For a detailed documentation see the related header file.
 */
GTI_ANALYSIS_RETURN
OpenMPlocks::notifyInit(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id)
{
    /* If a lock with this specific unique ID is initialized already, an error
     * message will be generated and the analysis fails. */
    if (myTracker.initialized(wait_id)) {
        myLogger->createMessage(
            MUST_ERROR_OPENMP,
            pId,
            lId,
            MustErrorMessage,
            "Lock is already initialized and must not be initialized again.");
        return GTI_ANALYSIS_FAILURE;
    }
    /* As this lock has not been initialized already, add its ID to the map
     * marking it as initialized.
     *
     * NOTE: The lock's initial ParallelId is zero, as the lock is not acquired
     *       yet and thus can't have a ParallelId assigned. */
    myTracker.initialize(wait_id);

    return GTI_ANALYSIS_SUCCESS;
}

/* Notify the module about a lock being destroyed.
 *
 * For a detailed documentation see the related header file.
 */
GTI_ANALYSIS_RETURN OpenMPlocks::notifyDestroy(ompt_wait_id_t wait_id)
{
    const auto extension_ender = [=]() { myLockExtensions.end_extension(wait_id); };
    myLockExtensions.begin_extension(wait_id);
    // Ensure ending the lock extension at end of scope (even on exceptions).
    const auto deferred = ScopeGuard<decltype(extension_ender)>{extension_ender};

    /* Search for the lock ID in the map of initialized locks and remove it.
     *
     * NOTE: If the lock is not initialized, no error message will be generated,
     *       as the checkInitialized analysis function will generate one and
     *       should be used in conjunction with this analysis function. */
    myTracker.destroy(wait_id);
    return GTI_ANALYSIS_SUCCESS;
}

/* Notify the module about a lock being acquired.
 *
 * For a detailed documentation see the related header file.
 */
GTI_ANALYSIS_RETURN
OpenMPlocks::notifyAcquired(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id)
{
    myLockExtensions.begin_extension(wait_id);
    /* To mark a specific lock as acquired, search for the lock ID in the map of
     * initialized locks and link it with the ParallelId of the process and
     * thread, which has acquired the lock. It will be checked in the release
     * event.
     *
     * NOTE: If the lock is not initialized, no error message will be generated,
     *       as the checkInitialized analysis function will generate one and
     *       should be used in conjunction with this analysis function. */
    myTracker.acquire(wait_id, pId);

    return GTI_ANALYSIS_SUCCESS;
}

/* Notify the module about a lock being released.
 *
 * For a detailed documentation see the related header file.
 */
GTI_ANALYSIS_RETURN
OpenMPlocks::notifyRelease(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id)
{
    // Ensure ending the lock extension at end of scope (even on exceptions). RAII rocks :)
    const auto extension_ender = [=]() { myLockExtensions.end_extension(wait_id); };
    const auto deferred = ScopeGuard<decltype(extension_ender)>{extension_ender};

    /* Search for the lock ID in the map of initialized locks and check, whether
     * the ParallelId matches the one, which has acquired the lock. If the IDs
     * don't match, an error will be generated.
     *
     * NOTE: If the lock is not initialized, no error message will be generated,
     *       as the checkInitialized analysis function will generate one and
     *       should be used in conjunction with this analysis function. */
    if (!myTracker.initialized(wait_id)) {
        // Let OpenMPlocks::checkInitialized do the hard work.
        return GTI_ANALYSIS_SUCCESS;
    }

    if (myTracker.owner(wait_id) != pId) {
        myLogger->createMessage(
            MUST_ERROR_OPENMP,
            pId,
            lId,
            MustErrorMessage,
            "Lock was locked by other thread than the one who is unlocking.");
        return GTI_ANALYSIS_FAILURE;
    }
    myTracker.release(wait_id);

    return GTI_ANALYSIS_SUCCESS;
}

/* Check whether a specific lock is initialized.
 *
 * For a detailed documentation see the related header file.
 */
GTI_ANALYSIS_RETURN
OpenMPlocks::checkInitialized(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id)
{
    /* If no lock with this specific unique ID is initialized yet, an error
     * message will be generated and the analysis fails. */
    if (!myTracker.initialized(wait_id)) {
        myLogger->createMessage(
            MUST_ERROR_OPENMP,
            pId,
            lId,
            MustErrorMessage,
            "Operating on an uninitialized lock.");
        return GTI_ANALYSIS_FAILURE;
    }

    return GTI_ANALYSIS_SUCCESS;
}

OpenMPlocks::~OpenMPlocks()
{
    if (myLogger != nullptr) {
        destroySubModuleInstance(myLogger);
    }
}
