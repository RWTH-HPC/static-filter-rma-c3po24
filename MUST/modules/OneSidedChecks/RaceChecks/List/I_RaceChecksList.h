/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_RaceChecksList.h
 *       @see I_RaceChecksList
 *
 *  @date 28.05.2023
 *  @author Simon Schwitanski
 */

#include "GtiEnums.h"

#include "MustEnums.h"
#include "BaseIds.h"
#include "MustTypes.h"
#include "I_OriginRMAOp.h"

#ifndef I_RaceChecksList_H
#define I_RaceChecksList_H

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
class I_RaceChecksList : public gti::I_Module
{
  public:
    /**
     * Called from RMATrack module if an origin memory operation is started.
     *
     * @param rmaId id of started origin memory operation
     */
    virtual gti::GTI_ANALYSIS_RETURN originOpStart(MustRMAId rmaId) = 0;

    /**
     * Called from RMATrack module if origin memory operations are completed.
     *
     * @param pId parallel id of associated completion call
     * @param lId location of associated completion call
     * @param rmaId array of ids of started origin memory operation
     * @param rmaIdLen length of array
     */
    virtual gti::GTI_ANALYSIS_RETURN
    originOpComplete(MustParallelId pId, MustLocationId lId, MustRMAId* rmaId, int rmaIdLen) = 0;

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
     * Track local memory access
     *
     * @param pId parallel id of associated local memory access
     * @param pc program counter at the memory access
     * @param isRead 1 = load, 0 = store
     * @param addr memory address that is accessed
     * @param count how many bytes were accessed
     */
    virtual gti::GTI_ANALYSIS_RETURN
    tsanAccess(MustParallelId pId, void* pc, int8_t isRead, void* addr, int64_t count) = 0;

    /**
     * Track local memory access, but as in bulk as array not one by one
     *
     * @param pId parallel id of associated local memory accesses
     * @param readPc array of program counters of reading accesses (with length `readPcLen`)
     * @param readPcNum array of number of pc send for each reading access (with length `readLen`)
     * @param readAdrr array of mem addresses of reading accesses (with length `readLen`)
     * @param readSize array of how many bytes were read (with length `readLen`)
     * @param readLen amount of reading accesses
     * @param readPcLen amount of program counters for reading accesses
     * @param writePc array of program counters of writing accesses (with length `writePcLen`)
     * @param writePcNum array of number of pc send for each writing access (with length `writeLen`)
     * @param writeAdrr array of mem addresses of writing accesses (with length `writeLen`)
     * @param writeSize array of how many bytes were written (with length `writeLen`)
     * @param writeLen amount of writing accesses
     * @param writePcLen amount of program counters for writing accesses
     */
    virtual gti::GTI_ANALYSIS_RETURN tsanAccessBulk(
        MustParallelId pId,
        void** readPc,
        size_t* readPcNum,
        void** readStartAddr,
        void** readEndAddr,
        size_t readLen,
        size_t readPcLen,
        void** writePc,
        size_t* writePcNum,
        void** writeStartAddr,
        void** writeEndAddr,
        size_t writeLen,
        size_t writePcLen) = 0;

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

    /**
     * Track MPI_Win_fence calls
     *
     * @param pId parallel context
     * @param lId location id of context.
     * @param assert assert type
     * @param win window the fence is associated with
     */
    virtual gti::GTI_ANALYSIS_RETURN
    winFence(MustParallelId pId, MustLocationId lId, int assert, MustWinType win) = 0;

}; /*class I_RaceChecksList*/

#endif /*I_RaceChecksList_H*/
