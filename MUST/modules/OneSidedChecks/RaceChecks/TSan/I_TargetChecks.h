/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_TargetChecks.h
 *       @see I_TargetChecks
 *
 *  @date 29.06.2017
 *  @author Simon Schwitanski
 */

#include "GtiEnums.h"

#include "MustEnums.h"
#include "BaseIds.h"
#include "MustTypes.h"
#include "I_OriginRMAOp.h"

#ifndef I_TARGETCHECKS_H
#define I_TARGETCHECKS_H

/**
 * Interface for an RMA operation tracking module.
 *
 * Dependencies (in listed order):
 * - ParallelIdAnalysis
 * - CreateMessage
 * - ArgumentAnalysis
 * - DatatypeTrack
 * - RequestTrack
 * - WinTrack
 */
class I_TargetChecks : public gti::I_Module
{
  public:
    /**
     * Called from RMATrack module if an target memory operation is started.
     *
     * @param rmaId id of started target memory operation
     */
    virtual gti::GTI_ANALYSIS_RETURN targetOpStart(MustRMAId rmaId) = 0;

    /**
     * Called from RMATrack module if target memory operations are completed.
     *
     * @param pId parallel id of associated completion call
     * @param lId location of associated completion call
     * @param rmaId array of ids of started target memory operation
     * @param rmaIdLen length of array
     */
    virtual gti::GTI_ANALYSIS_RETURN
    targetOpComplete(MustParallelId pId, MustLocationId lId, MustRMAId* rmaId, int rmaIdLen) = 0;

    /**
     * Track window creation.
     *
     * @param win window the lock is associated with
     * @param lock TSan identifier of window creation call
     */
    virtual gti::GTI_ANALYSIS_RETURN winCreate(MustWinType win, void* ann) = 0;

    /**
     * Track lock calls on windows.
     *
     * @param pId parallel context
     * @param lId location id of context.
     * @param lock_type type of lock (exclusive or shared)
     * @param rank target rank of lock
     * @param win window the lock is associated with
     * @param lock TSan identifier of the lock
     */
    virtual gti::GTI_ANALYSIS_RETURN winLock(
        MustParallelId pId,
        MustLocationId lId,
        int lock_type,
        int rank,
        MustWinType win,
        void* ann) = 0;

}; /*class I_TargetChecks*/

#endif /*I_TARGETCHECKS_H*/
