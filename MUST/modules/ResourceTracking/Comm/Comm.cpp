/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file Comm.cpp
 *       @see Comm.cpp
 *
 *  @date 23.06.2011
 *  @author Tobias Hilbrich, Joachim Protze, Mathias Korepkat
 */

#include "Comm.h"

using namespace must;

//=============================
// Constructor (invalid)
//=============================
Comm::Comm()
    : HandleInfoBase("Comm"), myIsNull(true), myIsPredefined(false),
      myPredefined(MUST_MPI_COMM_UNKNOWN), myPredefinedName(""), myIsCartesian(false),
      myIsGraph(false), myIsIntercomm(false), myContextId(0), myNextContextId(1), myGroup(NULL),
      myRemoteGroup(NULL), myCreationPId(0), myCreationLId(0), myReorder(false), myNdims(0),
      myDims(NULL), myPeriods(NULL), myNnodes(0), myIndices(NULL), myEdges(NULL), myCommTrack(NULL)
{
    // Nothing to do
}

//=============================
// Constructor (with pointers to reachable rank interval)
//=============================
Comm::Comm(I_CommTrack* commTrack)
    : HandleInfoBase("Comm"), myIsNull(true), myIsPredefined(false),
      myPredefined(MUST_MPI_COMM_UNKNOWN), myPredefinedName(""), myIsCartesian(false),
      myIsGraph(false), myIsIntercomm(false), myContextId(0), myNextContextId(1), myGroup(NULL),
      myRemoteGroup(NULL), myCreationPId(0), myCreationLId(0), myReorder(false), myNdims(0),
      myDims(NULL), myPeriods(NULL), myNnodes(0), myIndices(NULL), myEdges(NULL),
      myCommTrack(commTrack)
{
    // Nothing to do
}

//=============================
// Destructor
//=============================
Comm::~Comm()
{
    if (myGroup)
        myGroup->erase();
    myGroup = NULL;

    if (myRemoteGroup)
        myRemoteGroup->erase();
    myRemoteGroup = NULL;

    if (myDims)
        delete[] myDims;
    myDims = NULL;

    if (myPeriods)
        delete[] myPeriods;
    myPeriods = NULL;

    if (myIndices)
        delete[] myIndices;
    myIndices = NULL;

    if (myEdges)
        delete[] myEdges;
    myEdges = NULL;
}

//=============================
// isNull
//=============================
bool Comm::isNull(void) const { return myIsNull; }

//=============================
// isPredefined
//=============================
bool Comm::isPredefined(void) const { return myIsPredefined; }

//=============================
// isCartesian
//=============================
bool Comm::isCartesian(void) const { return myIsCartesian; }

//=============================
// isGraph
//=============================
bool Comm::isGraph(void) const { return myIsGraph; }

//=============================
// isIntercomm
//=============================
bool Comm::isIntercomm(void) const { return myIsIntercomm; }

//=============================
// group
//=============================
must::I_GroupTable* Comm::getGroup(void) const { return myGroup; }

//=============================
// getRemoteGroup
//=============================
must::I_GroupTable* Comm::getRemoteGroup(void) const { return myRemoteGroup; }

//=============================
// getContextId
//=============================
unsigned long long Comm::getContextId(void) { return myContextId; }

//=============================
// getNextContextId
//=============================
unsigned long long Comm::getNextContextId(void) { return myNextContextId++; }

//=============================
// getCreationPId
//=============================
MustParallelId Comm::getCreationPId(void) { return myCreationPId; }

//=============================
// getCreationLId
//=============================
MustLocationId Comm::getCreationLId(void) { return myCreationLId; }

//=============================
// getReorder
//=============================
bool Comm::getReorder(void) { return myReorder; }

//=============================
// getNdims
//=============================
int Comm::getNdims(void) { return myNdims; }

//=============================
// getDims
//=============================
int* Comm::getDims(void) { return myDims; }

//=============================
// getPeriods
//=============================
bool* Comm::getPeriods(void) { return myPeriods; }

//=============================
// getNnodes
//=============================
int Comm::getNnodes(void) { return myNnodes; }

//=============================
// getIndices
//=============================
int* Comm::getIndices(void) { return myIndices; }

//=============================
// getEdges
//=============================
int* Comm::getEdges(void) { return myEdges; }

//=============================
// getPredefinedInfo
//=============================
MustMpiCommPredefined Comm::getPredefinedInfo(void) { return myPredefined; }

//=============================
// getPredefinedInfo
//=============================
std::string Comm::getPredefinedName(void) { return myPredefinedName; }

//=============================
// compareComms
//=============================
bool Comm::compareComms(I_Comm* other)
{
    // must::cout << "COMPARE: r1=" << rank1 << " comm1=" << comm1 << " r2=" << rank2 << " comm2="
    // << comm2 << std::endl;

    if (isIntercomm() != other->isIntercomm())
        return false;

    if (!isIntercomm()) {
        // For intracomms
        if (getGroup() == other->getGroup() && getContextId() == other->getContextId())
            return true;
    } else {
        // For intercomms
        if (getContextId() != other->getContextId())
            return false;

        if ((getGroup() == other->getGroup() && getRemoteGroup() == other->getRemoteGroup()) ||
            (getGroup() == other->getRemoteGroup() && getRemoteGroup() == other->getGroup()))
            return true;
    }

    return false;
}

//=============================
// operator ==
//=============================
bool Comm::operator==(I_Comm& other)
{
    if (this == &other)
        return true;

    return compareComms(&other);
}

//=============================
// operator !=
//=============================
bool Comm::operator!=(I_Comm& other) { return !(*this == other); }

//=============================
// isRankReachable
//=============================
bool Comm::isRankReachable(int rank)
{
    if (isNull() || !myCommTrack) {
        return false;
    }
    return myCommTrack->isRankReachable(this, rank);
}

//=============================
// printInfo
//=============================
bool Comm::printInfo(
    std::stringstream& out,
    std::list<std::pair<MustParallelId, MustLocationId>>* pReferences)
{
    // Is Null
    if (isNull()) {
        out << "MPI_COMM_NULL";
        return true;
    }

    // Is Predefined
    if (isPredefined()) {
        out << myPredefinedName;
        return true;
    }

    // A user defined communicator
    pReferences->push_back(std::make_pair(getCreationPId(), getCreationLId()));
    out << "Communicator created at reference  " << pReferences->size();

    if (getGroup()) {
        out << " size=" << getGroup()->getSize();
    }

    //== Cartesian
    if (isCartesian()) {
        out << ", has a cartesian topology ndims=" << getNdims() << " reorder=" << getReorder()
            << " dims={";
        for (int i = 0; i < getNdims(); i++) {
            if (i != 0)
                out << ", ";
            out << (getDims())[i];
        }

        out << "} periods={";

        for (int i = 0; i < getNdims(); i++) {
            if (i != 0)
                out << ", ";
            out << (getPeriods())[i];
        }

        out << "}";
    }

    //== Graph
    if (isGraph()) {
        out << ", has a graph topology nnodes=" << getNnodes() << " reorder=" << getReorder()
            << " indices={";
        for (int i = 0; i < getNnodes(); i++) {
            if (i != 0)
                out << ", ";
            out << (getIndices())[i];
        }

        out << "}";
        /*edges={";

            for (int i = 0; i < indices[nnodes-1]; i++)
            {
                if (i != 0) out << ", ";
                out << edges[i];
            }

            out << "}";*/
    }

    //== Intercomm
    if (isIntercomm()) {
        out << ", is an intercommunicator";

        if (getRemoteGroup()) {
            out << " remote group has size=" << getRemoteGroup()->getSize();
        }
    }

    return true;
}

//=============================
// getResourceName
//=============================
std::string Comm::getResourceName(void) { return "Comm"; }

/*EOF*/
