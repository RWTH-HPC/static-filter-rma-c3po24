/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_RequestChecks.h
 *       @see I_RequestChecks.
 *
 *  @date 05.04.2011
 *  @author Mathias Korepkat
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"
#include "I_RequestTrack.h"

#ifndef I_REQUESTCHECKS_H
#define I_REQUESTCHECKS_H

/**
 * Interface for correctnesschecks of requests.
 *
 * Dependencies (order as listed):
 * - ParallelIdAnalysis
 * - CreateMessage
 * - ArgumentAnalysis
 * - RequestTrack
 *
 */
class I_RequestChecks : public gti::I_Module
{
  public:
    /**
     * Checks if a request is unknown,
     * manifests as error
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param request to check.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    errorIfNotKnown(MustParallelId pId, MustLocationId lId, int aId, MustRequestType request) = 0;

    /**
     * Checks if a request is null,
     * manifests as error
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param request to check.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    errorIfNull(MustParallelId pId, MustLocationId lId, int aId, MustRequestType request) = 0;

    /**
     * Checks if a request is persistent but inactive,
     * manifests as error
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param request to check.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN errorIfPersistentButInactive(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType request) = 0;

    /**
     * Checks if a request is canceled,
     * manifests as warning
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param request to check.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    warningIfCanceled(MustParallelId pId, MustLocationId lId, int aId, MustRequestType request) = 0;

    /**
     * Checks if a request is NULL or not active,
     * manifests as warning
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param request to check.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN warningIfNullOrInactive(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType request) = 0;

    /**
     * Checks if a request is an active receive request ,
     * manifests as warning
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param request  to check.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN warningIfActiveRecv(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType request) = 0;

    /**
     * Checks if a request is active,
     * manifests as error
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param request  to check.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    errorIfActive(MustParallelId pId, MustLocationId lId, int aId, MustRequestType request) = 0;

    /**
     * Checks if there is any active request in a request array,
     * manifests as error
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param requests to check.
     * @param size length of request array.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN errorIfActiveArray(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType* requests,
        int size) = 0;

    /**
     * Checks if any request in a request array is unknown,
     * manifests as error
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param requests to check.
     * @param size length of request array.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN errorIfNotKnownArray(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType* requests,
        int size) = 0;

    /**
     * Checks if a request array contains only inactive or null requests,
     * manifests as warning
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param requests to check.
     * @param size length of request array.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN warningIfNullOrInactiveArray(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType* requests,
        int size) = 0;

    /**
     * Checks if any request of a request array is null,
     * manifests as error
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param aId argument Id of the value to check.
     * @param requests to check.
     * @param size length of request array.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN errorIfNullArray(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType* requests,
        int size) = 0;

    /** Checks if partition is already active e.g. in MPI_Preay call.
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param request to query for.
     * @param partition to be marked active.
     */
    virtual gti::GTI_ANALYSIS_RETURN errorIfPartitionActive(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int partition) = 0;

    /** Checks if partitions in range are already active.
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param request to query for.
     * @param partition_low lowest to be marked active.
     * @param partition_high highest to be marked active.
     */
    virtual gti::GTI_ANALYSIS_RETURN errorIfPartitionActiveRange(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int partition_low,
        int partition_high) = 0;

    /** Checks if partitions in list are already active.
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param request to query for.
     * @param length of array of partitions.
     * @param array_of_partitions to be marked active.
     */
    virtual gti::GTI_ANALYSIS_RETURN errorIfPartitionActiveList(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int length,
        const int array_of_partitions[]) = 0;

    /** Checks if the list of partitions for MPI_Pready_list contains duplicates.
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param request to query for.
     * @param length of array of partitions.
     * @param array_of_partitions to be searched for duplicates.
     */
    virtual gti::GTI_ANALYSIS_RETURN errorIfPreadyListContainsDuplicates(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int length,
        const int array_of_partitions[]) = 0;

    /** Checks if the list of partitions for MPI_Pready_list has elements not within zero and
     *   the number of partitions.
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param request to query for.
     * @param length of array of partitions.
     * @param array_of_partitions to be checked.
     **/
    virtual gti::GTI_ANALYSIS_RETURN errorIfListNotWithinRangeZeroAndPartitionCount(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int length,
        const int array_of_partitions[]) = 0;

    /** Checks if the partition in MPI_Pready is within zero and
     *   the number of partitions.
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param request to query for.
     * @param partition to be checked.
     **/
    virtual gti::GTI_ANALYSIS_RETURN errorIfNotWithinRangeZeroAndPartitionCount(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int partition) = 0;

    /** Checks if the range given in MPI_Pready_range is within zero and
     *	the number of partitions.
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param request to query for.
     * @param partition_low lower bound to be checked.
     * @param partition_high upper bound to be checked.
     **/
    virtual gti::GTI_ANALYSIS_RETURN errorIfLbUbNotWithinRangeZeroAndPartitionCount(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int partition_low,
        int partition_high) = 0;

    /** Checks if the given request belongs to a partitioned send operation, for MPI_Pready*.
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param request to query for.
     **/
    virtual gti::GTI_ANALYSIS_RETURN errorIfNotPartitionedSendRequest(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request) = 0;

    /** Checks if the given request belongs to a partitioned receive operation, for MPI_Parrived.
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param request to query for.
     **/
    virtual gti::GTI_ANALYSIS_RETURN errorIfNotPartitionedReceiveRequest(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request) = 0;

}; /*class I_RequestChecks*/

#endif /*I_REQUESTCHECKS_H*/
