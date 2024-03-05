/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file RequestChecks.h
 *       @see MUST::RequestChecks.
 *
 *  @date 05.04.2011
 *  @author Mathias Korepkat
 */

#include "ModuleBase.h"
#include "I_ParallelIdAnalysis.h"
#include "I_ArgumentAnalysis.h"
#include "I_CreateMessage.h"

#include "I_RequestChecks.h"

#include <string>

#ifndef REQUESTCHECKS_H
#define REQUESTCHECKS_H

using namespace gti;

namespace must
{
/**
 * RequestChecks for correctness checks interface implementation.
 */
class RequestChecks : public gti::ModuleBase<RequestChecks, I_RequestChecks>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    RequestChecks(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~RequestChecks(void);

    /**
     * @see I_RequestChecks::errorIfNotKnown.
     */
    GTI_ANALYSIS_RETURN
    errorIfNotKnown(MustParallelId pId, MustLocationId lId, int aId, MustRequestType request);

    /**
     * @see I_RequestChecks::errorIfNull.
     */
    GTI_ANALYSIS_RETURN
    errorIfNull(MustParallelId pId, MustLocationId lId, int aId, MustRequestType request);

    /**
     * @see I_RequestChecks::errorIfPersistentButInactive.
     */
    GTI_ANALYSIS_RETURN errorIfPersistentButInactive(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType request);

    /**
     * @see I_RequestChecks::warningIfNullOtInactive.
     */
    GTI_ANALYSIS_RETURN warningIfNullOrInactive(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType request);

    /**
     * @see I_RequestChecks::warningIfActiveRecv.
     */
    GTI_ANALYSIS_RETURN
    warningIfActiveRecv(MustParallelId pId, MustLocationId lId, int aId, MustRequestType request);

    /**
     * @see I_RequestChecks::warningIfCanceled.
     */
    GTI_ANALYSIS_RETURN
    warningIfCanceled(MustParallelId pId, MustLocationId lId, int aId, MustRequestType request);

    /**
     * @see I_RequestChecks::errorIfActive.
     */
    GTI_ANALYSIS_RETURN
    errorIfActive(MustParallelId pId, MustLocationId lId, int aId, MustRequestType request);

    /**
     * @see I_RequestChecks::errorIfActiveArray.
     */
    GTI_ANALYSIS_RETURN errorIfActiveArray(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType* requests,
        int size);

    /**
     * @see I_RequestChecks::errorIfNotKnownArray.
     */
    GTI_ANALYSIS_RETURN errorIfNotKnownArray(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType* requests,
        int size);

    /**
     * @see I_RequestChecks::warningIfNullOtInactiveArray.
     */
    GTI_ANALYSIS_RETURN warningIfNullOrInactiveArray(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType* requests,
        int size);

    /**
     * @see I_RequestChecks::errorIfNullArray.
     */
    GTI_ANALYSIS_RETURN errorIfNullArray(
        MustParallelId pId,
        MustLocationId lId,
        int aId,
        MustRequestType* requests,
        int size);

    /**
     * @see I_RequestChecks::errorIfPartitionActive.
     */
    GTI_ANALYSIS_RETURN errorIfPartitionActive(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int partition);

    /**
     * @see I_RequestChecks::errorIfPartitionActiveRange.
     */
    GTI_ANALYSIS_RETURN errorIfPartitionActiveRange(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int partition_low,
        int partition_high);

    /**
     * @see I_RequestChecks::errorIfPartitionActiveList.
     */
    GTI_ANALYSIS_RETURN errorIfPartitionActiveList(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int length,
        const int array_of_partitions[]);

    /**
     * @see I_RequestChecks::errorIfPreadyListContainsDuplicates
     */
    GTI_ANALYSIS_RETURN
    errorIfPreadyListContainsDuplicates(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int length,
        const int array_of_partitions[]);

    /**
     * @see I_RequestChecks::errorIfListNotWithinRangeZeroAndPartitionCount
     */
    GTI_ANALYSIS_RETURN
    errorIfListNotWithinRangeZeroAndPartitionCount(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int length,
        const int array_of_partitions[]);

    /**
     * @see I_RequestChecks::errorIfNotWithinRangeZeroAndPartitionCount
     */
    GTI_ANALYSIS_RETURN errorIfNotWithinRangeZeroAndPartitionCount(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int partition);

    /**
     * @see I_RequestChecks::errorIfLbUbNotWithinRangeZeroAndPartitionCount
     */
    GTI_ANALYSIS_RETURN
    errorIfLbUbNotWithinRangeZeroAndPartitionCount(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int partition_low,
        int partition_high);

    /**
     * @see I_RequestChecks::errorIfNotPartitionedSendRequest
     */
    GTI_ANALYSIS_RETURN errorIfNotPartitionedSendRequest(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request);

    /**
     * @see I_RequestChecks::errorIfNotPartitionedReceiveRequest
     */
    GTI_ANALYSIS_RETURN errorIfNotPartitionedReceiveRequest(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request);

  protected:
    I_ParallelIdAnalysis* myPIdMod;
    I_CreateMessage* myLogger;
    I_ArgumentAnalysis* myArgMod;
    I_RequestTrack* myReqMod;
};
} // namespace must

#endif /*REQUESTCHECKS_H*/
