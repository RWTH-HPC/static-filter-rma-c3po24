/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef I_MUST_OpenMP_locks_H
#define I_MUST_OpenMP_locks_H

#include "BaseIds.h"
#include "GtiEnums.h"
#include "ModuleBase.h"
#include "omp-tools.h"

namespace must
{
/**
 * Interface for the OpenMP lock analysis module.
 *
 * The OpenMP lock analysis checks, whether locks are used properly inside the
 * application.
 */
class I_OpenMPlocks : public gti::I_Module
{
  public:
    /**
     * Notify the module about a new lock being initialized.
     *
     * If the lock is initialized for the first time, its @p wait_id should be
     * stored for later comparison. Duplicate initializations for the same @p
     * wait_id should trigger an error.
     *
     *
     * @param pId Rank and thread of the call.
     * @param lId Source location of the call.
     * @param wait_id Unique ID of the lock being initialized.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    notifyInit(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id) = 0;

    /**
     * Notify the module about an initialized lock being destroyed.
     *
     * Triggering this function should remove the @p wait_id from the modules
     * storage declaring the related lock as not initialized.
     *
     * @note This analysis function should be used in conjunction with @ref
     *       checkInitialized to check whether an uninitialized lock is used.
     *
     *
     * @param wait_id Unique ID of the lock being destroyed.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN notifyDestroy(ompt_wait_id_t wait_id) = 0;

    /**
     * Notify the module about a lock has been acquired.
     *
     * This method associates a specific lock with a given @p wait_id with the
     * @p pId of the thread, that has acquire the lock. This can be used to
     * check, whether the same thread is locking and unlocking the same lock.
     *
     * @note This analysis function should be used in conjunction with @ref
     *       checkInitialized to check whether an uninitialized lock is used.
     *
     *
     * @param pId Rank and thread of the call.
     * @param lId Source location of the call.
     * @param wait_id Unique ID of the lock that has been acquired.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    notifyAcquired(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id) = 0;

    /**
     * Notify the module about a lock being released.
     *
     * This method checks, whether a specific lock has been acquired by the same
     * thread, that is unlocking it now.
     *
     * @note This analysis function should be used in conjunction with @ref
     *       checkInitialized to check whether an uninitialized lock is used.
     *
     *
     * @param pId Rank and thread of the call.
     * @param lId Source location of the call.
     * @param wait_id Unique ID of the lock that will be released.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    notifyRelease(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id) = 0;

    /**
     * Check whether a lock is initialized.
     *
     * This function checks the status of a specific lock, i.e. to check whether
     * it is initialized at usage time.
     *
     *
     * @param pId Rank and thread of the call.
     * @param lId Source location of the call.
     * @param wait_id Unique ID of the lock being checked.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    checkInitialized(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id) = 0;
};
} // namespace must

#endif
