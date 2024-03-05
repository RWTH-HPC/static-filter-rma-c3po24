/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_TransferStatistics.h
 *       @see I_TransferStatistics.
 *
 *  @date 16.09.2011
 *  @author Tobias Hilbrich, Joachim Protze, Mathias Korepkat
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"

#ifndef I_TRANSFERSTATISTICS_H
#define I_TRANSFERSTATISTICS_H

/**
 * Module to capture communication statistics over time intervals.
 * An interval starts at a possibly globally synchronizing collective
 * and ends at one. In worst case these are MPI_Init and MPI_Finalize.
 *
 * Dependencies (order as listed):
 * - ParallelIdAnalysis
 * - CommTrack
 * - DatatypeTrack
 * - CollectiveCondition
 *
 */
class I_TransferStatistics : public gti::I_Module
{
  public:
    /**
     * Lets us know which rank we are and when the init is done.
     *
     * @param pId which rank are we?
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN init(MustParallelId pId) = 0;

    /**
     * Notification of a send event.
     *
     * @param type used in the transfer.
     * @param count used to repeat type.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    sendP2P(MustParallelId pId, MustDatatypeType type, int count) = 0;

    /**
     * Notification of a collective that performs no communication event.
     *
     * @param parallel id of context.
     * @param comm used in the collective.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN collNoTransfer(MustParallelId pId, MustCommType comm) = 0;

    /**
     * Notification of a collective that performs a send to a single rank.
     *
     * @param parallel id of context.
     * @param comm used in the collective.
     * @param type used in the transfer.
     * @param count used to repeat type.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    collSend(MustParallelId pId, MustCommType comm, MustDatatypeType type, int count) = 0;

    /**
     * Notification of a collective that performs a send to all tasks.
     *
     * @param parallel id of context.
     * @param comm used in the collective.
     * @param type used in the transfer.
     * @param count used to repeat type.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    collSendN(MustParallelId pId, MustCommType comm, MustDatatypeType type, int count) = 0;

    /**
     * Notification of a collective that performs a send to all tasks
     * with differing counts.
     *
     * @param parallel id of context.
     * @param comm used in the collective.
     * @param type used in the transfer.
     * @param counts used to repeat type.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    collSendCounts(MustParallelId pId, MustCommType comm, MustDatatypeType type, int* counts) = 0;

    /**
     * Notification of a collective that performs a send to all tasks
     * with differing types and counts.
     *
     * @param parallel id of context.
     * @param comm used in the collective.
     * @param types used in the transfer.
     * @param counts used to repeat type.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    collSendTypes(MustParallelId pId, MustCommType comm, MustDatatypeType* types, int* counts) = 0;

    /**
     * Used for collectives where only some ranks send (Bcast, scatter, scatterv).
     * This matches up the send call of the root process to create a complete wave of events.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    collRecvBalance(MustParallelId pId, MustCommType comm, int coll, int root) = 0;

}; /*class I_TransferStatistics*/

#endif /*I_TRANSFERSTATISTICS_H*/
