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

#include "PartitionedRequest.h"
#include <algorithm>
#include <cstdint>
#include <utility>

using namespace must;

//=============================
// Request
//=============================
PartitionedRequest::PartitionedRequest() : myNumPartitions(0), myMap(0), myActivePartitions(0)
{
    // Nothing to do
}

//=============================
// ~RequestRequest
//=============================
PartitionedRequest::~PartitionedRequest()
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
// isPartitionActive
//=============================
bool PartitionedRequest::isPartitionActive(int i)
{
    if (!isPartitioned()) {
        return false;
    }
    if (myActivePartitions.size() == 0) {
        return myMap[i];
    } else {
        return (
            myActivePartitions[i].first != (uint64_t)-1 &&
            myActivePartitions[i].second != (uint64_t)-1);
    }
}

//=============================
// getNumPartitions
//=============================
int PartitionedRequest::getNumPartitions() { return myNumPartitions; }

//=============================
// setPartitionActive
//=============================
void PartitionedRequest::setPartitionActive(int partition, MustParallelId pId, MustLocationId lId)
{
    if (myActivePartitions.size() == 0) {
        myMap.set(partition);
    } else {
        myActivePartitions.at(partition) = std::make_pair(pId, lId);
    }
}

//=============================
// getActivePartitionInfo
//=============================
std::pair<MustParallelId, MustLocationId> PartitionedRequest::getActivePartitionInfo(int i)
{
    return myActivePartitions[i];
}

//=============================
// isSend
//=============================
bool PartitionedRequest::isSend(void) { return myIsSend; }

//
// getBitmap
Bitmap& PartitionedRequest::getBitmap(void) { return myMap; }

//=============================
// getBitmapCount
//=============================
int PartitionedRequest::getBitmapCount() { return myMap.count(); }

//=============================
// clearBitmap
//=============================
void PartitionedRequest::clearBitmap() { myMap.clearAll(); }

//=============================
// printConflictPartitions
//=============================
bool PartitionedRequest::printConflictPartitions(
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
    const char* separator = "";
    if (myActivePartitions.empty()) { // standard output
        out << "Calling MPI_Pready_* on active partition(s)(";
        for (auto x : conflictPartitions) {
            out << separator << x;
            separator = ",";
        }
        out << ") is erroneous. ";
    } else { // output when must:partitioned_verbose_report is used
        out << "Found the following already active partitions, which is erroneous:" << std::endl;
        int counter = 0;
        for (const auto& x : conflictPartitions) {
            if (counter == 10) {
                out << ". ";
                return true;
            }
            // query if the <pId,lId> of this partition is already inside pReferences
            auto itr = std::find(pReferences->begin(), pReferences->end(), myActivePartitions[x]);
            int refnum;
            if (itr == pReferences->end()) { // if not, add it to the list
                pReferences->push_back(myActivePartitions[x]);
                refnum = pReferences->size();
            } else { // if it is, find the index of the pair
                refnum = std::distance(pReferences->begin(), itr) + 1;
            }

            out << "- partition " << x << " marked active at reference " << refnum << std::endl;
            counter++;
        }
    }
    return true;
}

//=============================
// printInfo
//=============================
bool PartitionedRequest::printInfo(
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
std::string PartitionedRequest::getResourceName(void) { return "Request"; }

/*EOF*/
