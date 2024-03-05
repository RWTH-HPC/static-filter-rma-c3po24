/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MUST_OpenMP_locks_H
#define MUST_OpenMP_locks_H

#include "I_OpenMPlocks.h"

#include "I_CreateMessage.h"
#include "LockExtensions.hpp"
#include "LockTracker.hpp"
#include "ModuleBase.h"
#include "omp-tools.h"
#include <cstdint>
#include <map>

namespace must
{
/**
 * OpenMP lock analysis module.
 *
 * This module tracks the initialization status of locks and raises errors, if
 * uninitialized locks are used in the application.
 */
class OpenMPlocks : public gti::ModuleBase<OpenMPlocks, I_OpenMPlocks>
{
  public:
    /**
     * Constructor.
     *
     *
     * @param instanceName name of this module instance.
     */
    OpenMPlocks(const char* instanceName);

    ~OpenMPlocks() override;

    /**
     * @copydoc I_OpenMPlocks::notifyInit
     */
    gti::GTI_ANALYSIS_RETURN
    notifyInit(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id) override;

    /**
     * @copydoc I_OpenMPlocks::notifyDestroy
     */
    gti::GTI_ANALYSIS_RETURN notifyDestroy(ompt_wait_id_t wait_id) override;

    /**
     * @copydoc I_OpenMPlocks::notifyAcquired
     */
    gti::GTI_ANALYSIS_RETURN
    notifyAcquired(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id) override;

    /**
     * @copydoc I_OpenMPlocks::notifyRelease
     */
    gti::GTI_ANALYSIS_RETURN
    notifyRelease(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id) override;

    /**
     * @copydoc I_OpenMPlocks::checkInitialized
     */
    gti::GTI_ANALYSIS_RETURN
    checkInitialized(MustParallelId pId, MustLocationId lId, ompt_wait_id_t wait_id) override;

  private:
    /**
     * Message logger.
     */
    I_CreateMessage* myLogger;

    static openmp::LockTracker myTracker;
    /**
     * Map of locks currently initialized with the related ParallelId, that has
     * acquired the lock.
     */
    static openmp::LockExtensions myLockExtensions;
};
} // namespace must

#endif
