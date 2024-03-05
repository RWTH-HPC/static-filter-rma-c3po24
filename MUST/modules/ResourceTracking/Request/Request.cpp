/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_Request.cpp
 *       @see I_Request.
 *
 *  @date 18.07.2011
 *  @author Tobias Hilbrich, Joachim Protze, Mathias Korepkat
 */

#include "Request.h"
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <utility>

using namespace must;

//=============================
// Request
//=============================
Request::Request()
    : HandleInfoBase("Request"), myIsActive(false), myIsPartitioned(false), myIsPersistent(false),
      myIsSend(false), myIsNull(true), myIsCanceled(false), myIsProcNull(false),
      myKind(MUST_REQUEST_UNKNOWN), myInfo(0), myCount(0), myDatatype(NULL), myTag(0), myComm(NULL),
      myDestSource(0), mySendMode(MUST_STANDARD_SEND), myCreationPId(0), myCreationLId(0),
      myActivationPId(0), myActivationLId(0), myCancelPId(0), myCancelLId(0)
{
    // Nothing to do
}

//=============================
// ~RequestRequest
//=============================
Request::~Request()
{
    // Free datatype/comm of persistent requests
    if (myDatatype)
        myDatatype->erase();
    myDatatype = NULL;

    if (myComm)
        myComm->erase();
    myComm = NULL;
}

//=============================
// isActive
//=============================
bool Request::isActive(void) { return myIsActive; }

//=============================
// isPartitioned
//=============================
bool Request::isPartitioned(void) { return myIsPartitioned; }

//=============================
// isPartitionActive
//=============================
bool Request::isPartitionActive(int i) { return false; }

//=============================
// setPartitionActive
//=============================
void Request::setPartitionActive(int partition, MustParallelId pId, MustLocationId lId)
{
    // do nothing if not PartitionedRequest
}

//=============================
// getActivePartitionInfo
//=============================
std::pair<MustParallelId, MustLocationId> Request::getActivePartitionInfo(int i)
{
    return std::make_pair(-1, -1);
}

//=============================
// isPersistent
//=============================
bool Request::isPersistent(void) { return myIsPersistent; }

//=============================
// isSend
//=============================
bool Request::isSend(void) { return myIsSend; }

//=============================
// isNull
//=============================
bool Request::isNull(void) { return myIsNull; }

//=============================
// isCanceled
//=============================
bool Request::isCanceled(void) { return myIsCanceled; }

//=============================
// getCount
//=============================
int Request::getCount(void) { return myCount; }

//=============================
// getNumPartitions
//=============================
int Request::getNumPartitions(void) { return 0; }

//=============================
// getDatatype
//=============================
I_Datatype* Request::getDatatype(void) { return myDatatype; }

//=============================
// getDatatypeCopy
//=============================
I_DatatypePersistent* Request::getDatatypeCopy(void)
{
    if (myDatatype)
        myDatatype->copy();
    return myDatatype;
}

//=============================
// getTag
//=============================
int Request::getTag(void) { return myTag; }

//=============================
// clearBitmap
//=============================
void Request::clearBitmap(void)
{
    // does nothing in Request.cpp
}

//=============================
// getComm
//=============================
I_Comm* Request::getComm(void) { return myComm; }

//=============================
// getComm
//=============================
I_CommPersistent* Request::getCommCopy(void)
{
    if (myComm)
        myComm->copy();
    return myComm;
}

//=============================
// getSource
//=============================
int Request::getSource(void) { return myDestSource; }

//=============================
// getDest
//=============================
int Request::getDest(void) { return myDestSource; }

//=============================
// getSendMode
//=============================
MustSendMode Request::getSendMode(void) { return mySendMode; }

//=============================
// getCreationPId
//=============================
MustParallelId Request::getCreationPId(void) { return myCreationPId; }

//=============================
// getCreationLId
//=============================
MustLocationId Request::getCreationLId(void) { return myCreationLId; }

//=============================
// getActivationPId
//=============================
MustParallelId Request::getActivationPId(void) { return myActivationPId; }

//=============================
// getActivationLId
//=============================
MustLocationId Request::getActivationLId(void) { return myActivationLId; }

//=============================
// getCancelPId
//=============================
MustParallelId Request::getCancelPId(void) { return myCancelPId; }

//=============================
// getCancelLId
//=============================
MustLocationId Request::getCancelLId(void) { return myCancelLId; }

bool Request::printConflictPartitions(
    const std::vector<int>& conflictPartitions,
    MustParallelId pId,
    MustLocationId lId,
    std::stringstream& out,
    std::list<std::pair<MustParallelId, MustLocationId>>* pReferences)
{
    if (myIsNull) {
        out << "MPI_REQUEST_NULL";
        return true;
    }
    return false;
}

//=============================
// getBitmapCount
//=============================
int Request::getBitmapCount() { return 0; }

//=============================
// printInfo
//=============================
bool Request::printInfo(
    std::stringstream& out,
    std::list<std::pair<MustParallelId, MustLocationId>>* pReferences)
{
    // Is Null
    if (myIsNull) {
        out << "MPI_REQUEST_NULL";
        return true;
    }

    std::string kindName = "", kindNameCapital;
    switch (myKind) {
    case MUST_REQUEST_P2P:
        kindName = "point-to-point";
        kindNameCapital = "Point-to-point";
        break;
    case MUST_REQUEST_COLL:
        kindName = "collective";
        kindNameCapital = "Collective";
        break;
    case MUST_REQUEST_IO:
        kindName = "I/O";
        kindNameCapital = "I/O";
        break;
    case MUST_REQUEST_RMA:
        kindName = "remote memory access";
        kindNameCapital = "Remote memory access";
        break;
    case MUST_REQUEST_GREQUEST:
        kindName = "generalized";
        kindNameCapital = "Generalized";
        break;
    case MUST_REQUEST_UNKNOWN:
        kindName = "undefined";
        kindNameCapital = "Undefined";
        break;
    }

    // Is persistent
    if (myIsPersistent) {
        const char* reqType = "Persistent ";
        if (myIsPartitioned) {
            reqType = "Partitioned ";
        }
        pReferences->push_back(std::make_pair(myCreationPId, myCreationLId));
        out << reqType << kindName << " request created at reference " << pReferences->size();

        if (myIsActive || myIsCanceled)
            out << ", ";
    } else {
        out << kindNameCapital << " request ";
    }

    // Is (also) active
    if (myIsActive) {
        pReferences->push_back(std::make_pair(myActivationPId, myActivationLId));
        out << "activated at reference " << pReferences->size();
    }

    // Is (also) canceled
    if (myIsCanceled) {
        pReferences->push_back(std::make_pair(myCancelPId, myCancelLId));
        out << ", canceled at reference " << pReferences->size();
    }

    return true;
}

//=============================
// getResourceName
//=============================
std::string Request::getResourceName(void) { return "Request"; }

//=============================
// isProcNull
//=============================
bool Request::isProcNull(void) { return myIsProcNull; }

//=============================
// isProcNull
//=============================
MUST_REQUEST_KIND Request::getKind(void) { return myKind; }

/*EOF*/
