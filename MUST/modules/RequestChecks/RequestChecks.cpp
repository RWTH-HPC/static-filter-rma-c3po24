/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file RequestChecks.cpp
 *       @see MUST::RequestChecks.
 *
 *  @date 05.04.2011
 *  @author Mathias Korepkat, Tobias Hilbrich
 */

#include "GtiMacros.h"
#include "RequestChecks.h"
#include "MustEnums.h"
#include "PrefixedOstream.hpp"

#include <algorithm>
#include <cstdio>
#include <sstream>

using namespace must;

mGET_INSTANCE_FUNCTION(RequestChecks)
mFREE_INSTANCE_FUNCTION(RequestChecks)
mPNMPI_REGISTRATIONPOINT_FUNCTION(RequestChecks)

//=============================
// Constructor
//=============================
RequestChecks::RequestChecks(const char* instanceName)
    : gti::ModuleBase<RequestChecks, I_RequestChecks>(instanceName)
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUBMODULES 4
    if (subModInstances.size() < NUM_SUBMODULES) {
        must::cerr << "Module has not enough sub modules, check its analysis specification! ("
                   << __FILE__ << "@" << __LINE__ << ")" << std::endl;
        assert(0);
    }
    if (subModInstances.size() > NUM_SUBMODULES) {
        for (std::vector<I_Module*>::size_type i = NUM_SUBMODULES; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myPIdMod = (I_ParallelIdAnalysis*)subModInstances[0];
    myLogger = (I_CreateMessage*)subModInstances[1];
    myArgMod = (I_ArgumentAnalysis*)subModInstances[2];
    myReqMod = (I_RequestTrack*)subModInstances[3];

    // Initialize module data
    // Nothing to do
}

//=============================
// Destructor
//=============================
RequestChecks::~RequestChecks()
{
    if (myPIdMod)
        destroySubModuleInstance((I_Module*)myPIdMod);
    myPIdMod = NULL;

    if (myLogger)
        destroySubModuleInstance((I_Module*)myLogger);
    myLogger = NULL;

    if (myArgMod)
        destroySubModuleInstance((I_Module*)myArgMod);
    myArgMod = NULL;

    if (myReqMod)
        destroySubModuleInstance((I_Module*)myReqMod);
    myReqMod = NULL;
}

//=============================
// errorIfNotKnown
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfNotKnown(
    MustParallelId pId,
    MustLocationId lId,
    int aId,
    MustRequestType request)
{
    I_Request* info = myReqMod->getRequest(pId, request);

    if (info == NULL) {
        std::stringstream stream;
        stream << "Argument " << myArgMod->getIndex(aId) << " (" << myArgMod->getArgName(aId)
               << ") is a unknown request (neither a predefined nor a user request)!";

        myLogger
            ->createMessage(MUST_ERROR_REQUEST_NOT_KNOWN, pId, lId, MustErrorMessage, stream.str());
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfNull
//=============================
GTI_ANALYSIS_RETURN
RequestChecks::errorIfNull(MustParallelId pId, MustLocationId lId, int aId, MustRequestType request)
{
    I_Request* info = myReqMod->getRequest(pId, request);

    if (info != NULL && info->isNull()) {
        std::stringstream stream;
        stream << "Argument " << myArgMod->getIndex(aId) << " (" << myArgMod->getArgName(aId)
               << ") is MPI_REQUEST_NULL!";

        myLogger->createMessage(MUST_ERROR_REQUEST_NULL, pId, lId, MustErrorMessage, stream.str());
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// warningIfNullorInactive
//=============================
GTI_ANALYSIS_RETURN RequestChecks::warningIfNullOrInactive(
    MustParallelId pId,
    MustLocationId lId,
    int aId,
    MustRequestType request)
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;

    if (info != NULL && (info->isNull() || !info->isActive())) {
        int messageID;
        std::stringstream stream;
        stream << "Argument " << myArgMod->getIndex(aId) << " (" << myArgMod->getArgName(aId)
               << ")";

        if (info->isNull()) {
            stream << " is MPI_REQUEST_NULL";
            messageID = MUST_WARNING_REQUEST_NULL;
        } else {
            stream << " is not active";
            messageID = MUST_WARNING_REQUEST_INACTIVE;
            info->printInfo(stream, &refs);
        }

        stream << " was this intended?";

        myLogger->createMessage(messageID, pId, lId, MustWarningMessage, stream.str(), refs);
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfNotKnownArray
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfNotKnownArray(
    MustParallelId pId,
    MustLocationId lId,
    int aId,
    MustRequestType* requests,
    int size)
{
    std::stringstream stream;
    bool error = false;
    I_Request* info;

    // Check all array entries
    for (int i = 0; i < size; i++) {
        info = myReqMod->getRequest(pId, requests[i]);

        if (info == NULL) {
            if (!error) {
                stream << "Argument " << myArgMod->getIndex(aId) << " ("
                       << myArgMod->getArgName(aId)
                       << ") has to be an array of predefined or user defined requests, the "
                          "following entries are unknown requests: ";
                error = true;
            } else {
                stream << ", ";
            }
            stream << myArgMod->getArgName(aId) << "[" << i << "]";
        }
    }

    if (error) {
        stream << ").";
        myLogger->createMessage(
            MUST_ERROR_REQUEST_NOT_KNOWN_ARRAY,
            pId,
            lId,
            MustErrorMessage,
            stream.str());
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// warningIfNullOrInactiveArray
//=============================
GTI_ANALYSIS_RETURN RequestChecks::warningIfNullOrInactiveArray(
    MustParallelId pId,
    MustLocationId lId,
    int aId,
    MustRequestType* requests,
    int size)
{
    std::stringstream stream;
    bool warn = true;
    I_Request* info;

    if (size == 0)
        return GTI_ANALYSIS_SUCCESS;

    // Check all array entries
    for (int i = 0; i < size; i++) {
        info = myReqMod->getRequest(pId, requests[i]);

        if (info != NULL && !info->isNull() && info->isActive()) {
            warn = false;
            break;
        }
    }

    if (warn) {
        stream << "Argument " << myArgMod->getIndex(aId) << " (" << myArgMod->getArgName(aId)
               << ") is an array of requests where all request are either in-active or null, was "
                  "this intended? ";
        myLogger->createMessage(
            MUST_WARNING_REQUEST_NULL_OR_INACTIVE_ARRAY,
            pId,
            lId,
            MustWarningMessage,
            stream.str());
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfNullArray
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfNullArray(
    MustParallelId pId,
    MustLocationId lId,
    int aId,
    MustRequestType* requests,
    int size)
{
    std::stringstream stream;
    bool error = false;
    I_Request* info;

    // Check all array entries
    for (int i = 0; i < size; i++) {
        info = myReqMod->getRequest(pId, requests[i]);

        if (info != NULL && info->isNull()) {
            if (!error) {
                stream << "Argument " << myArgMod->getIndex(aId) << " ("
                       << myArgMod->getArgName(aId)
                       << ") has to be an array of user defined requests, however, the following "
                          "entries are MPI_REQUEST_NULL: ";
                error = true;
            } else {
                stream << ", ";
            }
            stream << myArgMod->getArgName(aId) << "[" << i << "]";
        }
    }

    if (error) {
        stream << ").";
        myLogger->createMessage(
            MUST_ERROR_REQUEST_NULL_ARRAY,
            pId,
            lId,
            MustErrorMessage,
            stream.str());
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfPersistentButInactive
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfPersistentButInactive(
    MustParallelId pId,
    MustLocationId lId,
    int aId,
    MustRequestType request)
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;

    if (info != NULL && !info->isNull() && info->isPersistent() && !info->isActive()) {
        std::stringstream stream;
        stream << "Argument " << myArgMod->getIndex(aId) << " (" << myArgMod->getArgName(aId)
               << ") is a persistent but in-active request! ";

        info->printInfo(stream, &refs);

        myLogger->createMessage(
            MUST_ERROR_REQUEST_PERSISTENT_BUT_INACTIVE,
            pId,
            lId,
            MustErrorMessage,
            stream.str(),
            refs);
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// warningIfCanceled
//=============================
GTI_ANALYSIS_RETURN RequestChecks::warningIfCanceled(
    MustParallelId pId,
    MustLocationId lId,
    int aId,
    MustRequestType request)
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;

    if (info != NULL && !info->isNull() && info->isCanceled()) {
        std::stringstream stream;
        stream << "Argument " << myArgMod->getIndex(aId) << " (" << myArgMod->getArgName(aId)
               << ") was already marked for cancelation! ";

        info->printInfo(stream, &refs);

        myLogger->createMessage(
            MUST_WARNING_REQUEST_CANCELED,
            pId,
            lId,
            MustWarningMessage,
            stream.str(),
            refs);
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// warningIfActiveRecv
//=============================
GTI_ANALYSIS_RETURN RequestChecks::warningIfActiveRecv(
    MustParallelId pId,
    MustLocationId lId,
    int aId,
    MustRequestType request)
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    if (info && info->isActive() && !info->isSend()) {
        std::stringstream stream;
        stream << "Argument " << myArgMod->getIndex(aId) << " (" << myArgMod->getArgName(aId)
               << ") is an active receive request, that should never be freed as the receiver will"
               << " have no way to verify that the receive has completed.";

        info->printInfo(stream, &refs);

        myLogger->createMessage(
            MUST_WARNING_REQUEST_ACTIVE_RECV,
            pId,
            lId,
            MustWarningMessage,
            stream.str(),
            refs);
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfActive
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfActive(
    MustParallelId pId,
    MustLocationId lId,
    int aId,
    MustRequestType request)
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;

    if (info != NULL && !info->isNull() && info->isActive()) {
        std::stringstream stream;

        stream << "Argument " << myArgMod->getIndex(aId) << " (" << myArgMod->getArgName(aId)
               << ") is already an active request! ";

        info->printInfo(stream, &refs);

        myLogger->createMessage(
            MUST_ERROR_REQUEST_ACTIVE,
            pId,
            lId,
            MustErrorMessage,
            stream.str(),
            refs);
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfActiveArray
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfActiveArray(
    MustParallelId pId,
    MustLocationId lId,
    int aId,
    MustRequestType* request,
    int size)
{
    I_Request* info;
    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    bool error = false;
    std::stringstream stream;

    for (int i = 0; i < size; i++) {
        info = myReqMod->getRequest(pId, request[i]);

        if (info != NULL && !info->isNull() && info->isActive()) {
            if (!error) {
                stream << "Argument " << myArgMod->getIndex(aId) << " ("
                       << myArgMod->getArgName(aId)
                       << ") has to be an array of non-active requests, the following entries are "
                          "already active: ";
                error = true;
            } else {
                stream << ", ";
            }
            stream << myArgMod->getArgName(aId) << "[" << i << "] (";
            info->printInfo(stream, &refs);
            stream << ")";
        }
    }

    if (error) {
        myLogger->createMessage(
            MUST_ERROR_REQUEST_ACTIVE_ARRAY,
            pId,
            lId,
            MustErrorMessage,
            stream.str(),
            refs);

        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfPartitionActive
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfPartitionActive(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType request,
    int partition)
{
    // std::cout << "RequestChecks:errorIfPartitionActive called from Pready." << std::endl;
    return errorIfPartitionActiveList(pId, lId, request, 1, &partition);
}

//=============================
// errorIfPartitionActiveRange
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfPartitionActiveRange(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType request,
    int partition_low,
    int partition_high)
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    if (info == NULL || info->isNull()) {
        return GTI_ANALYSIS_FAILURE;
    }
    std::vector<int> activePartitions;
    for (int partition = partition_low; partition <= partition_high; ++partition) {
        if (info->isPartitionActive(partition)) {
            activePartitions.push_back(partition);
        }
    }
    if (activePartitions.empty() == false) {
        std::stringstream stream;
        info->printConflictPartitions(activePartitions, pId, lId, stream, &refs);
        info->printInfo(stream, &refs);

        myLogger->createMessage(
            MUST_ERROR_REQUEST_PARTITION_ACTIVE,
            pId,
            lId,
            MustErrorMessage,
            stream.str(),
            refs);
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfPartitionActiveList
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfPartitionActiveList(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType request,
    int length,
    const int array_of_partitions[])
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    if (info == NULL || info->isNull()) {
        return GTI_ANALYSIS_FAILURE;
    }
    std::stringstream stream;
    std::vector<int> activePartitions;
    for (int x = 0; x < length; ++x) {
        if (info->isPartitionActive(array_of_partitions[x])) {
            activePartitions.push_back(array_of_partitions[x]);
        }
    }
    if (!activePartitions.empty()) {
        info->printConflictPartitions(activePartitions, pId, lId, stream, &refs);
        info->printInfo(stream, &refs);

        myLogger->createMessage(
            MUST_ERROR_REQUEST_PARTITION_ACTIVE,
            pId,
            lId,
            MustErrorMessage,
            stream.str(),
            refs);
        return GTI_ANALYSIS_FAILURE;
    }

    return GTI_ANALYSIS_SUCCESS;
}

GTI_ANALYSIS_RETURN
RequestChecks::errorIfPreadyListContainsDuplicates(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType request,
    int length,
    const int array_of_partitions[])
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    int array[length];
    int i;
    for (i = 0; i < length; ++i) {
        array[i] = array_of_partitions[i];
    }
    std::sort(array, array + length);
    for (i = 0; i < length - 1; ++i) {
        if (array[i] == array[i + 1]) {
            std::stringstream stream;
            stream << "There is at least one duplicate in MPI_Pready_list call."
                   << " Found duplicate of value " << array[i] << " in {" << array_of_partitions[0];
            for (i = 1; i < length - 1; ++i) {
                stream << "," << array_of_partitions[i];
            }
            stream << "}.";

            info->printInfo(stream, &refs);

            myLogger->createMessage(
                MUST_ERROR_REQUEST_PARTITION_ACTIVE,
                pId,
                lId,
                MustErrorMessage,
                stream.str(),
                refs);
            return GTI_ANALYSIS_FAILURE;
        }
    }

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfListNotWithinRangeZeroAndPartitionCount
//=============================
GTI_ANALYSIS_RETURN
RequestChecks::errorIfListNotWithinRangeZeroAndPartitionCount(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType request,
    int length,
    const int array_of_partitions[])
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    std::stringstream stream;
    int max = info->getNumPartitions() - 1;
    stream << "Found element(s) out of range 0 and numPartitions(" << max << ") in MPI_Pready*: ";
    int i = 0;
    bool printMessage = false;
    for (i = 0; i < length; ++i) {
        if (array_of_partitions[i] < 0 || array_of_partitions[i] > max) {
            stream << "value " << array_of_partitions[i] << " at index " << i << ". ";
            printMessage = true;
        }
    }
    if (printMessage) {
        info->printInfo(stream, &refs);

        myLogger->createMessage(
            MUST_ERROR_REQUEST_PARTITION_ACTIVE,
            pId,
            lId,
            MustErrorMessage,
            stream.str(),
            refs);
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfNotWithinRangeZeroAndPartitionCount
//=============================
GTI_ANALYSIS_RETURN
RequestChecks::errorIfNotWithinRangeZeroAndPartitionCount(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType request,
    int partition)
{
    return errorIfListNotWithinRangeZeroAndPartitionCount(pId, lId, request, 1, &partition);
}

//=============================
// errorIfLbUbNotWithinRangeZeroAndPartitionCount
//=============================
GTI_ANALYSIS_RETURN
RequestChecks::errorIfLbUbNotWithinRangeZeroAndPartitionCount(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType request,
    int partition_low,
    int partition_high)
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    std::stringstream stream;
    GTI_ANALYSIS_RETURN res = GTI_ANALYSIS_SUCCESS;
    bool printMessage = false;
    if (partition_low > partition_high) {
        stream << "Found that partition_low(" << partition_low << ") is bigger than"
               << " partition_high(" << partition_high << "). ";
        printMessage = true;
        res = GTI_ANALYSIS_FAILURE;
    }
    int max = info->getNumPartitions() - 1;
    if (partition_low < 0 || partition_low > max) {
        printMessage = true;
        res = GTI_ANALYSIS_FAILURE;
        stream << "The lower bound for MPI_Pready_range is not within range 0 and " << max
               << ": partition_low=" << partition_low << ". ";
    }
    if (partition_high < 0 || partition_high > max) {
        printMessage = true;
        res = GTI_ANALYSIS_FAILURE;
        stream << "The upper bound for MPI_Pready_range is not within range 0 and " << max
               << ": partition_high=" << partition_high << ". ";
    }
    if (printMessage) {
        info->printInfo(stream, &refs);
        myLogger->createMessage(
            MUST_ERROR_REQUEST_PARTITION_ACTIVE,
            pId,
            lId,
            MustErrorMessage,
            stream.str(),
            refs);
    }
    return res;
}

//=============================
// errorIfNotPartitionedSendRequest
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfNotPartitionedSendRequest(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType request)
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    std::stringstream stream;
    bool printMessage = false;
    if (info == NULL || info->isNull()) {
        return GTI_ANALYSIS_FAILURE;
    }
    if (info->isPartitioned() == false) {
        stream << "The given request object does not correspond to a partitioned operation, which "
                  "is erroneous. ";
        printMessage = true;
    } else if (info->isSend() == false) {
        stream << "The given request object does not correspond to a partitioned send operation, "
                  "which is erroneous. ";
        printMessage = true;
    }
    if (printMessage) {
        info->printInfo(stream, &refs);
        myLogger->createMessage(
            MUST_ERROR_REQUEST_NOT_PARTITIONED_SEND,
            pId,
            lId,
            MustErrorMessage,
            stream.str(),
            refs);
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// errorIfNotPartitionedReceiveRequest
//=============================
GTI_ANALYSIS_RETURN RequestChecks::errorIfNotPartitionedReceiveRequest(
    MustParallelId pId,
    MustLocationId lId,
    MustRequestType request)
{
    I_Request* info = myReqMod->getRequest(pId, request);
    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    std::stringstream stream;
    bool printMessage = false;
    if (info == NULL || info->isNull()) {
        return GTI_ANALYSIS_FAILURE;
    }
    if (info->isPartitioned() == false) {
        stream << "The given request object does not correspond to a partitioned operation, which "
                  "is erroneous. ";
        printMessage = true;
    } else if (info->isSend()) {
        stream << "The given request object does not correspond to a partitioned receive "
                  "operation, which is erroneous. ";
        printMessage = true;
    }
    if (printMessage) {
        info->printInfo(stream, &refs);
        myLogger->createMessage(
            MUST_ERROR_REQUEST_NOT_PARTITIONED_RECV,
            pId,
            lId,
            MustErrorMessage,
            stream.str(),
            refs);
        return GTI_ANALYSIS_FAILURE;
    }
    return GTI_ANALYSIS_SUCCESS;
}

/*EOF*/
