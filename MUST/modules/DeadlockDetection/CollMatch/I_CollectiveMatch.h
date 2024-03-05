/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_CollectiveMatch.h
 *       @see I_CollectiveMatch.
 *
 *  @date 29.07.2011
 *  @author Tobias Hilbrich, Joachim Protze, Mathias Korepkat,
 */

#include "I_Module.h"
#include "GtiEnums.h"

#include "BaseIds.h"
#include "MustEnums.h"
#include "MustTypes.h"

#include "I_CollMatchListener.h"

#ifndef I_COLLECTIVEMATCH_H
#define I_COLLECTIVEMATCH_H

/**
 * Interface for collective matching.
 *
 * Suspension and operation queuing is implemented
 * with I_OperationReordering and is also used to impose
 * orderings relating from analyzing only feasible operations
 * for deadlock detection.
 *
 * Dependencies (order as listed):
 * - ParallelIdAnalysis
 * - BaseConstants
 * - CreateMessage  ##for printing collective mismatches (that is easier than to print this later on
 * as a deadlock)
 * - CommTrack
 * - DatatypeTrack
 * - OpTrack
 * - OperationReordering
 *
 */
class I_CollectiveMatch : public gti::I_Module
{
  public:
    /*
     * Collective for implicit collectives like
     * MPI_Finalize and communicator constructors.
     *
     * @param pId parallel id of calling context.
     * @param lId location id of calling context.
     * @param coll id of the collective, @see must::MustCollCommType.
     * @param comm used for collective.
     * @param numTasks used for reduction counts progress of reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN CollNoTransfer(
        MustParallelId pId,
        MustLocationId lId,
        int coll, // formerly gti::MustCollCommType
        MustCommType comm,
        int numTasks, // counter for event aggregation
        int hasRequest) = 0;

    /*
     * A send operation to a single process. Usually performed by the slaves
     * (and possibly also the root) of a collective with a root process.
     *
     * The following preconditioned collective transfers are mapped to this:
     * Must_Coll_Send
     * Must_Coll_Op_Send
     *
     * @param pId parallel id of calling context.
     * @param lId location id of calling context.
     * @param coll id of the collective, @see must::MustCollCommType.
     * @param comm used for collective.
     * @param count for send type repetition.
     * @param type used for the data transfer.
     * @param dest root to send to (as a rank in MPI_COMM_WORLD).
     * @param hasOp true iff this transfer uses a reduction operation.
     * @param op operation to use for reduction, only significant if hasOp == true.
     * @param numTasks used for reduction counts progress of reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN CollSend(
        MustParallelId pId,
        MustLocationId lId,
        int coll, // formerly gti::MustCollCommType
        int count,
        MustDatatypeType type,
        int dest, /*Root process to send to as a rank in MPI_COMM_WORLD*/
        MustCommType comm,
        int hasOp,
        MustOpType op,
        int numTasks, // counter for event aggregation
        int hasRequest) = 0;

    /*
     * A send operation to all tasks in the comm.
     * For MPI_Bcast this excludes the root.
     *
     * The following preconditioned collective transfers are mapped to this:
     * Must_Coll_Send_n
     * Must_Coll_Op_Send_n
     * Must_Coll_Send_buffers
     * Must_Coll_Op_Send_buffers
     *
     * @param pId parallel id of calling context.
     * @param lId location id of calling context.
     * @param coll id of the collective, @see must::MustCollCommType.
     * @param count for send type repetition.
     * @param type used for the data transfer.
     * @param commsize number of tasks to send to (size of comm).
     * @param comm used for collective.
     * @param hasOp true iff this transfer uses a reduction operation.
     * @param op operation to use for reduction, only significant if hasOp == true.
     * @param numTasks used for reduction counts progress of reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN CollSendN(
        MustParallelId pId,
        MustLocationId lId,
        int coll, // formerly gti::MustCollCommType
        int count,
        MustDatatypeType type,
        int commsize,
        MustCommType comm,
        int hasOp,
        MustOpType op,
        int numTasks,
        int hasRequest) = 0;

    /*
     * A send operation to all tasks in the comm. Each send can use
     * a different sendcount.
     *
     * The following preconditioned collective transfers are mapped to this:
     * Must_Coll_Send_counts
     * Must_Coll_Op_Send_counts
     *
     * @param pId parallel id of calling context.
     * @param lId location id of calling context.
     * @param coll id of the collective, @see must::MustCollCommType.
     * @param counts for send type repetition.
     * @param type used for the data transfer.
     * @param commsize number of tasks to send to (size of comm).
     * @param comm used for collective.
     * @param hasOp true iff this transfer uses a reduction operation.
     * @param op operation to use for reduction, only significant if hasOp == true.
     * @param numTasks used for reduction counts progress of reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN CollSendCounts(
        MustParallelId pId,
        MustLocationId lId,
        int coll, // formerly gti::MustCollCommType
        const int counts[],
        MustDatatypeType type,
        int commsize,
        MustCommType comm,
        int hasOp,
        MustOpType op,
        int numTasks,
        int hasRequest) = 0;

    /*
     * A send operation to all tasks in the comm. Each send can use
     * a different sendcount and type.
     *
     * The following preconditioned collective transfers are mapped to this:
     * Must_Coll_Send_types
     *
     * @param pId parallel id of calling context.
     * @param lId location id of calling context.
     * @param coll id of the collective, @see must::MustCollCommType.
     * @param counts for send type repetition.
     * @param types used for the data transfer.
     * @param commsize number of tasks to send to (size of comm).
     * @param comm used for collective.
     * @param numTasks used for reduction counts progress of reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN CollSendTypes(
        MustParallelId pId,
        MustLocationId lId,
        int coll, // formerly gti::MustCollCommType
        const int counts[],
        const MustDatatypeType types[],
        int commsize,
        MustCommType comm,
        int numTasks,
        int hasRequest) = 0;

    /*
     * A recv from the root of the collective, only used for collectives
     * that actually have a root.
     *
     * The following preconditioned collective transfers are mapped to this:
     * PMust_Coll_Recv
     *
     * @param pId parallel id of calling context.
     * @param lId location id of calling context.
     * @param coll id of the collective, @see must::MustCollCommType.
     * @param count for recv type repetition.
     * @param type used for the data transfer.
     * @param src root to receive from (as a rank in MPI_COMM_WORLD).
     * @param comm used for collective.
     * @param numTasks used for reduction counts progress of reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN CollRecv(
        MustParallelId pId,
        MustLocationId lId,
        int coll, // formerly gti::MustCollCommType
        int count,
        MustDatatypeType type,
        int src, /*Root process to receive from as a rank in MPI_COMM_WORLD*/
        MustCommType comm,
        int numTasks,
        int hasRequest) = 0;

    /*
     * A receive from each task in the communicator.
     *
     * The following preconditioned collective transfers are mapped to this:
     * Must_Coll_Recv_n
     * Must_Coll_Op_Recv_n
     * PMust_Coll_Recv_buffers
     *
     * @param pId parallel id of calling context.
     * @param lId location id of calling context.
     * @param coll id of the collective, @see must::MustCollCommType.
     * @param count for recv type repetition.
     * @param type used for the data transfer.
     * @param commsize number of tasks to receive from (size of comm).
     * @param comm used for collective.
     * @param hasOp true iff this transfer uses a reduction operation.
     * @param op operation to use for reduction, only significant if hasOp == true.
     * @param numTasks used for reduction counts progress of reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN CollRecvN(
        MustParallelId pId,
        MustLocationId lId,
        int coll, // formerly gti::MustCollCommType
        int count,
        MustDatatypeType type,
        int commsize,
        MustCommType comm,
        int hasOp,
        MustOpType op,
        int numTasks,
        int hasRequest) = 0;

    /**
     * A receive from each task in the communicator. Each receive can
     * have a distinct count.
     *
     * The following preconditioned collective transfers are mapped to this:
     * PMust_Coll_Recv_counts
     *
     * @param pId parallel id of calling context.
     * @param lId location id of calling context.
     * @param coll id of the collective, @see must::MustCollCommType.
     * @param counts for recv type repetition.
     * @param type used for the data transfer.
     * @param commsize number of tasks to receive from (size of comm).
     * @param comm used for collective.
     * @param numTasks used for reduction counts progress of reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN CollRecvCounts(
        MustParallelId pId,
        MustLocationId lId,
        int coll, // formerly gti::MustCollCommType
        const int counts[],
        MustDatatypeType type,
        int commsize,
        MustCommType comm,
        int numTasks,
        int hasRequest) = 0;

    /*
     * A receive from each task in the communicator. Each receive can
     * have a distinct count and type.
     *
     * The following preconditioned collective transfers are mapped to this:
     * PMust_Coll_Recv_types
     *
     * @param pId parallel id of calling context.
     * @param lId location id of calling context.
     * @param coll id of the collective, @see must::MustCollCommType.
     * @param counts for recv type repetition.
     * @param types used for the data transfer.
     * @param commsize number of tasks to receive from (size of comm).
     * @param comm used for collective.
     * @param numTasks used for reduction counts progress of reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN CollRecvTypes(
        MustParallelId pId,
        MustLocationId lId,
        int coll, // formerly gti::MustCollCommType
        const int counts[],
        const MustDatatypeType types[],
        int commsize,
        MustCommType comm,
        int numTasks,
        int hasRequest) = 0;

    /**
     * Registers the given callback with the I_CollectiveMatch implementation.
     * The listener is triggered whenever a new match was found and its
     * processing completed. It should be used to notify any module that may
     * be interested in matches. Multiple listeners may be registered and are
     * executed in the order in which they where registered.
     * @param listener to register.
     * @return GTI_SUCCESS if successful.
     */
    virtual gti::GTI_RETURN registerListener(must::I_CollMatchListener* listener) = 0;

    /**
     * Creates a checkpoint of the current matching status.
     * The I_CollectiveMatch::rollback function rewinds the matching state to
     * this checkpoint.
     * There may always only be one checkpoint, when this function is called
     * it overwrites any preceding checkpoint. Checkpoints are used to execute
     * matches that can be undone, e.g. if a wildcard source is missing.
     *
     * This mechanism will only work if I_OperationReordering,
     * I_P2PMatch and I_BlockingState are checkpointed at the same
     * time, otherwise inconsistent state results.
     */
    virtual void checkpoint(void) = 0;

    /**
     * Rolls the matching information back to the last checkpoint.
     *
     * This mechanism will only work if I_OperationReordering,
     * I_P2PMatch and I_BlockingState are rolled back at the same
     * time, otherwise inconsistent state results.
     */
    virtual void rollback(void) = 0;

}; /*class I_CollectiveMatch*/

#endif /*I_COLLECTIVEMATCH_H*/
