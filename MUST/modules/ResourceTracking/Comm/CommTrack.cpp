/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file CommTrack.cpp
 *       @see MUST::CommTrack.
 *
 *  @date 04.03.2011
 *  @author Tobias Hilbrich, Mathias Korepkat
 */

#include "GtiMacros.h"
#include "MustEnums.h"
#include "PrefixedOstream.hpp"

#include "CommTrack.h"
#include "I_Group.h"

#include <sstream>
#include <utility>
#include <tuple>
#include <pnmpimod.h>
#include <assert.h>

using namespace must;

mGET_INSTANCE_FUNCTION(CommTrack)
mFREE_INSTANCE_FUNCTION(CommTrack)
mPNMPI_REGISTRATIONPOINT_FUNCTION(CommTrack)

#define CONTEXTID_MULTIPLIER 128

//=============================
// Constructor
//=============================
CommTrack::CommTrack(const char* instanceName)
    : TrackBase<Comm, I_Comm, MustCommType, MustMpiCommPredefined, CommTrack, I_CommTrack>(
          instanceName),
      mySelfCommInfos(), myWorldCommInfos(), myReachableAvailable(false), myReachableBegin(-1),
      myReachableEnd(-1), myCommWorldHandles(), myRealCommWorld(0)
{
    // Get the GroupTrack module
    if (myFurtherMods.size() < 1) {
        must::cerr << "Error: the CommTrack module needs the GroupTrack module as a child, but it "
                      "was not specified."
                   << std::endl;
        assert(0);
    }

    myGroupMod = (I_GroupTrack*)myFurtherMods[0];

    // Retrieve function pointers for passing resources across
    getWrapAcrossFunction("passCommAcross", (GTI_Fct_t*)&myPassCommAcrossFunc);
    getWrapAcrossFunction("passFreeCommAcross", (GTI_Fct_t*)&myFreeCommAcrossFunc);
}

//=============================
// Destructor
//=============================
CommTrack::~CommTrack()
{
    // Notify HandleInfoBase of ongoing shutdown
    HandleInfoBase::disableFreeForwardingAcross();
    myGroupMod->notifyOfShutdown();

    // User handles and predefineds are freed by TrackBase, modules too.
    myGroupMod = NULL;

    // Free data
    for (std::vector<I_Module*>::size_type i = 0; i < mySelfCommInfos.size(); i++) {
        if (!mySelfCommInfos[i]->mpiErase()) {
#ifdef MUST_DEBUG
            must::cout << "Warning: A MPI_COMM_SELF handle was queried for in a persistent way and "
                          "was not freed."
                       << std::endl;
#endif
        }
    }

    for (std::vector<I_Module*>::size_type i = 0; i < myWorldCommInfos.size(); i++) {
        // IMPORTANT: some entries may be NULL, if this TBON node only receives from some ranks!
        Comm* c = myWorldCommInfos[i];
        if (c && !c->mpiErase()) {
#ifdef MUST_DEBUG
            must::cout << "Warning: A MPI_COMM_WORLD handle was queried for in a persistent way "
                          "and was not freed."
                       << std::endl;
#endif
        }
    }

    mySelfCommInfos.clear();
    myWorldCommInfos.clear();
    myCommWorldHandles.clear();
}

//=============================
// commGroup
//=============================
GTI_ANALYSIS_RETURN
CommTrack::commGroup(MustParallelId pId, MustLocationId lId, MustCommType comm, MustGroupType group)
{
    //==1) Find information for comm
    Comm* pInfo;
    pInfo = getCommInfo(pId2Rank(pId), comm);

    if (!pInfo || pInfo->isNull())
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't do
                                     // correctness checking here)

    //==3) Pass pId, lId, I_GroupTable, group (handle value) to the GroupTrack module
    myGroupMod->commGroup(
        pId,
        lId,
        pInfo->getGroup(),
        group); // This is also correct for inter-comms, there we must return the local group, which
                // we are doing here

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// commCreate
//=============================
GTI_ANALYSIS_RETURN CommTrack::commCreate(
    MustParallelId pId,
    MustLocationId lId,
    MustCommType comm,
    MustGroupType group,
    MustCommType newcomm)
{
    //==2) Get information for the old communicator, abort if unknown or null
    Comm* cInfo;
    cInfo = getCommInfo(pId2Rank(pId), comm);

    if (!cInfo || cInfo->isNull()) {
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't
                                     // do correctness checking here)
    }

    // Increment next context of old comm now, even if this task got COMM_NULL we have to increment
    // it
    cInfo->myNextContextId = cInfo->myNextContextId + 1;

    //==1) Get information for the group, abort for null or unknown groups
    I_Group* gInfo = myGroupMod->getGroup(pId, group);
    if (!gInfo || gInfo->isNull())
        return GTI_ANALYSIS_SUCCESS;

    //==3) Abort if new comm is null or a predefined value
    {
        auto s_safe_Predefineds = slock_safe_ptr(myPredefineds);
        if (s_safe_Predefineds->find(newcomm) != s_safe_Predefineds->end() ||
            newcomm == myNullValue)
            return GTI_ANALYSIS_SUCCESS;
    }

    //==4) Create the full information for the new comm
    Comm* newInfo = new Comm(this);

    newInfo->myIsNull = false;
    newInfo->myIsPredefined = false;
    newInfo->myIsCartesian = false;
    newInfo->myIsGraph = false;
    newInfo->myIsIntercomm = false;
    newInfo->myGroup = gInfo->getGroup();
    if (newInfo->myGroup)
        newInfo->myGroup->copy();
    newInfo->myRemoteGroup = NULL;
    newInfo->myCreationPId = pId;
    newInfo->myCreationLId = lId;

    // context
    newInfo->myContextId = cInfo->myNextContextId - 1;
    newInfo->myNextContextId = CONTEXTID_MULTIPLIER * cInfo->myNextContextId;

    //==4) Register the new user comm
    submitUserHandle(pId, newcomm, newInfo);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// commCreateFromGroup
//=============================
GTI_ANALYSIS_RETURN CommTrack::commCreateFromGroup(
    MustParallelId pId,
    MustLocationId lId,
    MustGroupType group,
    MustCommType newcomm)
{
    //==1) Get information for the group, abort for null or unknown groups
    I_Group* gInfo = myGroupMod->getGroup(pId, group);
    if (!gInfo || gInfo->isNull())
        return GTI_ANALYSIS_SUCCESS;

    //==3) Abort if new comm is null or a predefined value
    {
        auto s_safe_Predefineds = slock_safe_ptr(myPredefineds);
        if (s_safe_Predefineds->find(newcomm) != s_safe_Predefineds->end() ||
            newcomm == myNullValue)
            return GTI_ANALYSIS_SUCCESS;
    }

    //==4) Create the full information for the new comm
    Comm* newInfo = new Comm(this);

    newInfo->myIsNull = false;
    newInfo->myIsPredefined = false;
    newInfo->myIsCartesian = false;
    newInfo->myIsGraph = false;
    newInfo->myIsIntercomm = false;
    newInfo->myGroup = gInfo->getGroup();
    if (newInfo->myGroup)
        newInfo->myGroup->copy();
    newInfo->myRemoteGroup = NULL;
    newInfo->myCreationPId = pId;
    newInfo->myCreationLId = lId;

    // context
    newInfo->myContextId = 0;
    newInfo->myNextContextId = 1;

    //==4) Register the new user comm
    submitUserHandle(pId, newcomm, newInfo);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// commDup
//=============================
GTI_ANALYSIS_RETURN
CommTrack::commDup(MustParallelId pId, MustLocationId lId, MustCommType comm, MustCommType newcomm)
{
    //==1) Get information for the old communicator, abort if unknown or null
    Comm* cInfo;
    cInfo = getCommInfo(pId2Rank(pId), comm);

    if (!cInfo || cInfo->isNull()) {
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't do
                                     // correctness checking here)
    }

    // Increment next context of old comm now, even if this task got COMM_NULL we have to increment
    // it
    cInfo->myNextContextId = cInfo->myNextContextId + 1;

    //==2) Abort if new comm is null or a predefined value
    {
        auto s_safe_Predefineds = slock_safe_ptr(myPredefineds);
        if (s_safe_Predefineds->find(newcomm) != s_safe_Predefineds->end() ||
            newcomm == myNullValue)
            return GTI_ANALYSIS_SUCCESS;
    }

    //==3) Create the full information for the new comm
    Comm* newInfo = new Comm(this);

    newInfo->myIsNull = false;
    newInfo->myIsPredefined = false;
    newInfo->myIsCartesian = cInfo->myIsCartesian;
    newInfo->myIsGraph = cInfo->myIsGraph;
    newInfo->myIsIntercomm = cInfo->myIsIntercomm;
    newInfo->myGroup = cInfo->myGroup;
    if (newInfo->myGroup)
        newInfo->myGroup->copy();
    newInfo->myRemoteGroup = cInfo->myRemoteGroup;
    if (newInfo->myRemoteGroup)
        newInfo->myRemoteGroup->copy();
    newInfo->myCreationPId = pId;
    newInfo->myCreationLId = lId;

    // context
    newInfo->myContextId = cInfo->myNextContextId - 1;
    newInfo->myNextContextId = CONTEXTID_MULTIPLIER * cInfo->myNextContextId;

    // Cart stuff
    newInfo->myReorder = cInfo->myReorder;
    newInfo->myNdims = cInfo->myNdims;
    newInfo->myDims = new int[newInfo->myNdims];
    newInfo->myPeriods = new bool[newInfo->myNdims];
    for (int i = 0; i < newInfo->myNdims; i++) {
        newInfo->myDims[i] = cInfo->myDims[i];
        newInfo->myPeriods[i] = cInfo->myPeriods[i];
    }

    // Graph stuff
    newInfo->myNnodes = cInfo->myNnodes;
    newInfo->myIndices = new int[newInfo->myNnodes];
    int numIndices = 0;
    if (newInfo->myNnodes)
        numIndices = cInfo->myIndices[cInfo->myNnodes - 1];
    newInfo->myEdges = new int[numIndices];
    for (int i = 0; i < newInfo->myNnodes; i++)
        newInfo->myIndices[i] = cInfo->myIndices[i];
    for (int i = 0; i < numIndices; i++)
        newInfo->myEdges[i] = cInfo->myEdges[i];

    //==4) Register the new user comm
    submitUserHandle(pId, newcomm, newInfo);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// commFree
//=============================
GTI_ANALYSIS_RETURN CommTrack::commFree(MustParallelId pId, MustLocationId lId, MustCommType comm)
{
    // Apply the free
    removeUserHandle(pId, comm);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// commSplit
//=============================
GTI_ANALYSIS_RETURN CommTrack::commSplit(
    MustParallelId pId,
    MustLocationId lId,
    MustCommType comm,
    int color,
    int key,
    MustCommType newcomm,
    int newCommSize,
    int* newRank2WorldArray)
{
    //==1) Get information for the old communicator, abort if unknown or null
    Comm* cInfo;
    cInfo = getCommInfo(pId2Rank(pId), comm);

    if (!cInfo || cInfo->isNull()) {
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't
                                     // do correctness checking here)
    }

    // Increment next context of old comm now, even if this task got COMM_NULL we have to increment
    // it
    cInfo->myNextContextId = cInfo->myNextContextId + 1;

    //==2) Abort if new comm is null or a predefined value
    {
        auto s_safe_Predefineds = slock_safe_ptr(myPredefineds);
        if (s_safe_Predefineds->find(newcomm) != s_safe_Predefineds->end() ||
            newcomm == myNullValue)
            return GTI_ANALYSIS_SUCCESS;
    }

    //==2b) Create the set for the new group
    std::vector<int> group;
    group.resize(newCommSize);
    for (int i = 0; i < newCommSize; i++)
        group[i] = newRank2WorldArray[i];

    //==3) Create the full information for the new comm
    Comm* newInfo = new Comm(this);

    newInfo->myIsNull = false;
    newInfo->myIsPredefined = false;
    newInfo->myIsCartesian = false;
    newInfo->myIsGraph = false;
    newInfo->myIsIntercomm = false;
    newInfo->myGroup = myGroupMod->getGroupTable(group);
    newInfo->myRemoteGroup = NULL;
    newInfo->myCreationPId = pId;
    newInfo->myCreationLId = lId;

    // context
    newInfo->myContextId = cInfo->myNextContextId - 1;
    newInfo->myNextContextId = CONTEXTID_MULTIPLIER * cInfo->myNextContextId;

    //==4) Register the new user comm
    submitUserHandle(pId, newcomm, newInfo);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// graphCreate
//=============================
GTI_ANALYSIS_RETURN CommTrack::graphCreate(
    MustParallelId pId,
    MustLocationId lId,
    MustCommType comm,
    int nnodes,
    int nedges,
    const int* indices,
    const int* edges,
    int reorder,
    MustCommType newcomm,
    int newCommSize,
    int* newRank2WorldArray)
{
    //==1) Get information for the old communicator, abort if unknown or null
    Comm* cInfo;
    cInfo = getCommInfo(pId2Rank(pId), comm);

    if (!cInfo || cInfo->isNull()) {
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't
                                     // do correctness checking here)
    }

    // Increment next context of old comm now, even if this task got COMM_NULL we have to increment
    // it
    cInfo->myNextContextId = cInfo->myNextContextId + 1;

    //==2) Abort if new comm is null or a predefined value
    {
        auto s_safe_Predefineds = slock_safe_ptr(myPredefineds);
        if (s_safe_Predefineds->find(newcomm) != s_safe_Predefineds->end() ||
            newcomm == myNullValue)
            return GTI_ANALYSIS_SUCCESS;
    }

    //==2b) Create the set for the new group
    std::vector<int> group;
    group.resize(newCommSize);
    for (int i = 0; i < newCommSize; i++)
        group[i] = newRank2WorldArray[i];

    //==3) Create the full information for the new comm
    Comm* newInfo = new Comm(this);

    newInfo->myIsNull = false;
    newInfo->myIsPredefined = false;
    newInfo->myIsCartesian = false;
    newInfo->myIsGraph = true;
    newInfo->myIsIntercomm = false;
    newInfo->myGroup = myGroupMod->getGroupTable(group);
    newInfo->myRemoteGroup = NULL;
    newInfo->myCreationPId = pId;
    newInfo->myCreationLId = lId;

    // context
    newInfo->myContextId = cInfo->myNextContextId - 1;
    newInfo->myNextContextId = CONTEXTID_MULTIPLIER * cInfo->myNextContextId;

    // graph stuff
    newInfo->myNnodes = nnodes;
    newInfo->myIndices = new int[nnodes];
    newInfo->myEdges = new int[nedges];
    for (int i = 0; i < nnodes; i++)
        newInfo->myIndices[i] = indices[i];
    for (int i = 0; i < nedges; i++)
        newInfo->myEdges[i] = edges[i];
    newInfo->myReorder = reorder;

    //==4) Register the new user comm
    submitUserHandle(pId, newcomm, newInfo);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// distGraphCreate
//=============================
GTI_ANALYSIS_RETURN CommTrack::distGraphCreate(
    MustParallelId pId,
    MustLocationId lId,
    MustCommType comm,
    MustCommType newcomm,
    int newCommSize,
    int* newRank2WorldArray)
{
    //==1) Get information for the old communicator, abort if unknown or null
    Comm* cInfo;
    cInfo = getCommInfo(pId2Rank(pId), comm);

    if (!cInfo || cInfo->isNull()) {
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't
                                     // do correctness checking here)
    }

    // Increment next context of old comm now, even if this task got COMM_NULL we have to increment
    // it
    cInfo->myNextContextId = cInfo->myNextContextId + 1;

    //==2) Abort if new comm is null or a predefined value
    if (newcomm == myNullValue ||
        slock_safe_ptr(myPredefineds)->find(newcomm) != slock_safe_ptr(myPredefineds)->end())
        return GTI_ANALYSIS_SUCCESS;

    //==2b) Create the set for the new group
    std::vector<int> group;
    group.resize(newCommSize);
    for (int i = 0; i < newCommSize; i++)
        group[i] = newRank2WorldArray[i];

    //==3) Create the full information for the new comm
    Comm* newInfo = new Comm(this);

    newInfo->myIsNull = false;
    newInfo->myIsPredefined = false;
    newInfo->myIsCartesian = false;
    newInfo->myIsGraph = true;
    newInfo->myIsIntercomm = false;
    newInfo->myGroup = myGroupMod->getGroupTable(group);
    newInfo->myRemoteGroup = NULL;
    newInfo->myCreationPId = pId;
    newInfo->myCreationLId = lId;

    // context
    newInfo->myContextId = cInfo->myNextContextId - 1;
    newInfo->myNextContextId = CONTEXTID_MULTIPLIER * cInfo->myNextContextId;

    newInfo->myNnodes = 0;

    //==4) Register the new user comm
    submitUserHandle(pId, newcomm, newInfo);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// cartCreate
//=============================
GTI_ANALYSIS_RETURN CommTrack::cartCreate(
    MustParallelId pId,
    MustLocationId lId,
    MustCommType comm,
    int ndims,
    const int* dims,
    const int* periods,
    int reorder,
    MustCommType newcomm,
    int newCommSize,
    int* newRank2WorldArray)
{
    //==1) Get information for the old communicator, abort if unknown or null
    Comm* cInfo;
    cInfo = getCommInfo(pId2Rank(pId), comm);

    if (!cInfo || cInfo->isNull())
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't do
                                     // correctness checking here)

    // Increment next context of old comm now, even if this task got COMM_NULL we have to increment
    // it
    cInfo->myNextContextId = cInfo->myNextContextId + 1;

    //==2) Abort if new comm is null or a predefined value
    {
        auto s_safe_Predefineds = slock_safe_ptr(myPredefineds);
        if (s_safe_Predefineds->find(newcomm) != s_safe_Predefineds->end() ||
            newcomm == myNullValue)
            return GTI_ANALYSIS_SUCCESS;
    }

    //==2b) Create the set for the new group
    std::vector<int> group;
    group.resize(newCommSize);
    for (int i = 0; i < newCommSize; i++)
        group[i] = newRank2WorldArray[i];

    //==3) Create the full information for the new comm
    Comm* newInfo = new Comm(this);

    newInfo->myIsNull = false;
    newInfo->myIsPredefined = false;
    newInfo->myIsCartesian = true;
    newInfo->myIsGraph = false;
    newInfo->myIsIntercomm = false;
    newInfo->myGroup = myGroupMod->getGroupTable(group);
    newInfo->myRemoteGroup = NULL;
    newInfo->myCreationPId = pId;
    newInfo->myCreationLId = lId;

    // context
    newInfo->myContextId = cInfo->myNextContextId - 1;
    newInfo->myNextContextId = CONTEXTID_MULTIPLIER * cInfo->myNextContextId;

    // graph stuff
    newInfo->myNdims = ndims;
    newInfo->myDims = new int[ndims];
    newInfo->myPeriods = new bool[ndims];
    for (int i = 0; i < ndims; i++) {
        newInfo->myDims[i] = dims[i];
        newInfo->myPeriods[i] = (bool)periods[i];
    }
    newInfo->myReorder = reorder;

    //==4) Register the new user comm
    submitUserHandle(pId, newcomm, newInfo);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// cartSub
//=============================
GTI_ANALYSIS_RETURN CommTrack::cartSub(
    MustParallelId pId,
    MustLocationId lId,
    MustCommType comm,
    int ndims,
    const int* remain,
    MustCommType newcomm,
    int newCommSize,
    int* newRank2WorldArray)
{
    //==1) Get information for the old communicator, abort if unknown or null
    Comm* cInfo;
    cInfo = getCommInfo(pId2Rank(pId), comm);

    if (!cInfo || cInfo->isNull())
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't do
                                     // correctness checking here)

    // Increment next context of old comm now, even if this task got COMM_NULL we have to increment
    // it
    cInfo->myNextContextId = cInfo->myNextContextId + 1;

    //==2) Abort if new comm is null or a predefined value
    {
        auto s_safe_Predefineds = slock_safe_ptr(myPredefineds);
        if (s_safe_Predefineds->find(newcomm) != s_safe_Predefineds->end() ||
            newcomm == myNullValue)
            return GTI_ANALYSIS_SUCCESS;
    }

    //==2b) Create the set for the new group
    std::vector<int> group;
    group.resize(newCommSize);
    for (int i = 0; i < newCommSize; i++)
        group[i] = newRank2WorldArray[i];

    //==3) Create the full information for the new comm
    Comm* newInfo = new Comm(this);

    newInfo->myIsNull = false;
    newInfo->myIsPredefined = false;
    newInfo->myIsCartesian = true;
    newInfo->myIsGraph = false;
    newInfo->myIsIntercomm = false;
    newInfo->myGroup = myGroupMod->getGroupTable(group);
    newInfo->myRemoteGroup = NULL;
    newInfo->myCreationPId = pId;
    newInfo->myCreationLId = lId;

    // context
    newInfo->myContextId = cInfo->myNextContextId - 1;
    newInfo->myNextContextId = CONTEXTID_MULTIPLIER * cInfo->myNextContextId;

    newInfo->myNdims = 0;
    for (int i = 0; i < ndims; i++) {
        if (remain[i] != 0)
            newInfo->myNdims++;
    }

    // graph stuff
    //     newInfo->myNdims = ndims;
    newInfo->myDims = new int[newInfo->myNdims];
    newInfo->myPeriods = new bool[newInfo->myNdims];
    for (int i = 0, j = 0; i < ndims; i++) {
        if (remain[i] == 0)
            continue;
        newInfo->myDims[j] = cInfo->myDims[i];
        newInfo->myPeriods[j] = cInfo->myPeriods[i];
        j++;
    }
    newInfo->myReorder = cInfo->myReorder;

    //==4) Register the new user comm
    submitUserHandle(pId, newcomm, newInfo);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// intercommCreate
//=============================
GTI_ANALYSIS_RETURN CommTrack::intercommCreate(
    MustParallelId pId,
    MustLocationId lId,
    MustCommType local_comm,
    int local_leader,
    MustCommType peer_comm,
    int remote_leader,
    int tag,
    MustCommType newintercomm,
    int remoteGroupSize,
    int* remoteRank2WorldArray,
    int contextId)
{
    //==1) Get information for the local_comm, abort if unknown or null
    Comm* cInfo;
    cInfo = getCommInfo(pId2Rank(pId), local_comm);

    if (!cInfo || cInfo->isNull()) {
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't do
                                     // correctness checking here)
    }

    // Increment next context of old comm now, even if this task got COMM_NULL we have to increment
    // it
    cInfo->myNextContextId = cInfo->myNextContextId + 1;

    //==2) Abort if new comm is null or a predefined value
    {
        auto s_safe_Predefineds = slock_safe_ptr(myPredefineds);
        if (s_safe_Predefineds->find(newintercomm) != s_safe_Predefineds->end() ||
            newintercomm == myNullValue)
            return GTI_ANALYSIS_SUCCESS;
    }

    //==2b) Create the set for the remote group of the new comm
    std::vector<int> remoteGroup;
    remoteGroup.resize(remoteGroupSize);
    for (int i = 0; i < remoteGroupSize; i++)
        remoteGroup[i] = remoteRank2WorldArray[i];

    //==3) Create the full information for the new comm
    Comm* newInfo = new Comm(this);

    newInfo->myIsNull = false;
    newInfo->myIsPredefined = false;
    newInfo->myIsCartesian = false;
    newInfo->myIsGraph = false;
    newInfo->myIsIntercomm = true;
    newInfo->myRemoteGroup = myGroupMod->getGroupTable(remoteGroup);
    newInfo->myGroup = cInfo->myGroup;
    if (newInfo->myGroup)
        newInfo->myGroup->copy();
    newInfo->myCreationPId = pId;
    newInfo->myCreationLId = lId;

    // context
    /**
     * A communication is used in the wrappers to compute the context
     * id which should be global across the local and remote group. I have
     * totally no clue how to do it differently.
     */
    newInfo->myContextId = contextId;
    newInfo->myNextContextId = contextId + 1;

    //==4) Register the new user comm
    submitUserHandle(pId, newintercomm, newInfo);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// intercommMerge
//=============================
GTI_ANALYSIS_RETURN CommTrack::intercommMerge(
    MustParallelId pId,
    MustLocationId lId,
    MustCommType intercomm,
    int high,
    MustCommType newintracomm,
    int newCommSize,
    int* newRank2WorldArray)
{
    //==1) Get information for the local_comm, abort if unknown or null
    Comm* cInfo;
    cInfo = getCommInfo(pId2Rank(pId), intercomm);

    if (!cInfo || cInfo->isNull()) {
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't
                                     // do correctness checking here)
    }

    // Increment next context of old comm now, even if this task got COMM_NULL we have to increment
    // it
    cInfo->myNextContextId = cInfo->myNextContextId + 1;

    //==2) Abort if new comm is null or a predefined value
    {
        auto s_safe_Predefineds = slock_safe_ptr(myPredefineds);
        if (s_safe_Predefineds->find(newintracomm) != s_safe_Predefineds->end() ||
            newintracomm == myNullValue)
            return GTI_ANALYSIS_SUCCESS;
    }

    //==2b) Create the set for the group of the new comm
    std::vector<int> group;
    group.resize(newCommSize);
    for (int i = 0; i < newCommSize; i++)
        group[i] = newRank2WorldArray[i];

    //==3) Create the full information for the new comm
    Comm* newInfo = new Comm(this);

    newInfo->myIsNull = false;
    newInfo->myIsPredefined = false;
    newInfo->myIsCartesian = false;
    newInfo->myIsGraph = false;
    newInfo->myIsIntercomm = false;
    newInfo->myRemoteGroup = NULL;
    newInfo->myGroup = myGroupMod->getGroupTable(group);
    newInfo->myCreationPId = pId;
    newInfo->myCreationLId = lId;

    // context
    /**
     * @todo
     * This is really nasty, we may run into conflicts as the base for intercomm contexts
     * reaches into the space used for contexts of regular comms.
     * We try to shift a bit here, but this is not nice.
     */
    newInfo->myContextId = cInfo->myNextContextId - 1 + (CONTEXTID_MULTIPLIER / 2);
    newInfo->myNextContextId =
        CONTEXTID_MULTIPLIER * (cInfo->myNextContextId + CONTEXTID_MULTIPLIER / 2);

    //==4) Register the new user comm
    submitUserHandle(pId, newintracomm, newInfo);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// commRemoteGroup
//=============================
GTI_ANALYSIS_RETURN CommTrack::commRemoteGroup(
    MustParallelId pId,
    MustLocationId lId,
    MustCommType comm,
    MustGroupType newGroup)
{
    //==1) Find information for comm
    Comm* pInfo;
    pInfo = getCommInfo(pId2Rank(pId), comm);

    if (!pInfo || pInfo->isNull()) {
        return GTI_ANALYSIS_SUCCESS; // if not found or null handle, simply return (we don't
                                     // do correctness checking here)
    }

    if (!pInfo->isIntercomm()) {
        return GTI_ANALYSIS_SUCCESS; // this is not an intercomm, simply return (we don't do
                                     // correctness checking here)
    }

    //==3) Pass pId, lId, I_GroupTable, group (handle value) to the GroupTrack module
    myGroupMod->commGroup(pId, lId, pInfo->myRemoteGroup, newGroup);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// addRemoteComm
//=============================
GTI_ANALYSIS_RETURN CommTrack::addRemoteComm(
    int rank,
    int hasHandle,
    MustCommType commHandle,
    MustRemoteIdType remoteId,
    int isNull,
    int isPredefined,
    int predefinedEnum,
    int isCartesian,
    int isGraph,
    int isIntercomm,
    unsigned long long contextId,
    MustRemoteIdType groupTableId,
    MustRemoteIdType groupTableIdRemote,
    MustParallelId creationPId,
    MustLocationId creationLId,
    int reorder,
    int ndims,
    const int* dims,
    const int* periods,
    int nnodes,
    int nedges,
    const int* indices,
    const int* edges)
{
    // Create the resource
    Comm* resource = new Comm(this); // Well the reachable here is not really true ... TODO

    resource->myIsNull = isNull;
    resource->myIsPredefined = isPredefined;
    resource->myPredefined = (MustMpiCommPredefined)predefinedEnum;

    if (resource->myPredefined == MUST_MPI_COMM_SELF)
        resource->myPredefinedName = "MPI_COMM_SELF";
    if (resource->myPredefined == MUST_MPI_COMM_WORLD)
        resource->myPredefinedName = "MPI_COMM_WORLD";

    resource->myIsCartesian = isCartesian;
    resource->myIsGraph = isGraph;
    resource->myIsIntercomm = isIntercomm;

    resource->myContextId = contextId;
    resource->myNextContextId = 0; // Unimportant, we can't derive a new comm from this one

    if (groupTableId)
        resource->myGroup = myGroupMod->getGroupTable(rank, groupTableId);
    else
        resource->myGroup = NULL;

    if (groupTableId)
        resource->myRemoteGroup = myGroupMod->getGroupTable(rank, groupTableIdRemote);
    else
        resource->myRemoteGroup = NULL;

    resource->myCreationLId = creationLId;
    resource->myCreationPId = creationPId;

    resource->myReorder = reorder;

    resource->myNdims = ndims;
    resource->myDims = NULL;
    if (ndims > 0) {
        resource->myDims = new int[ndims];
        resource->myPeriods = new bool[ndims];

        for (int i = 0; i < ndims; i++) {
            resource->myDims[i] = dims[i];
            resource->myPeriods[i] = periods[i];
        }
    }

    resource->myNnodes = nnodes;
    resource->myIndices = NULL;
    resource->myEdges = NULL;

    if (nnodes > 0) {
        resource->myIndices = new int[nnodes];
        resource->myEdges = new int[nedges];

        for (int i = 0; i < nnodes; i++)
            resource->myIndices[i] = indices[i];

        for (int i = 0; i < nedges; i++)
            resource->myEdges[i] = edges[i];
    }

    // Register the new remote comm
    submitRemoteResource(rank, remoteId, hasHandle, commHandle, resource);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// freeRemoteComm
//=============================
GTI_ANALYSIS_RETURN CommTrack::freeRemoteComm(int rank, MustRemoteIdType remoteId)
{
    removeRemoteResource(rank, remoteId);
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// passCommAcross
//=============================
bool CommTrack::passCommAcross(MustParallelId pId, MustCommType comm, int toPlaceId)
{
    return passCommAcross(pId2Rank(pId), comm, toPlaceId);
}

//=============================
// passCommAcross
//=============================
bool CommTrack::passCommAcross(int rank, MustCommType commHandle, int toPlaceId)
{
    // Get comm
    Comm* comm = getCommInfo(rank, commHandle);

    // Use the existing passCommAcross
    return passCommAcrossInternal(rank, comm, toPlaceId, NULL, true, commHandle);
}

//=============================
// passCommAcross
//=============================
bool CommTrack::passCommAcross(
    int rank,
    I_Comm* commIn,
    int toPlaceId,
    MustRemoteIdType* pOutRemoteId)
{
    if (!commIn)
        return false; // Invalid comm

    // Cast to internal representation
    Comm* comm = (Comm*)commIn;

    // Do we still have a handle associated?
    MustCommType handle = 0;
    bool hasHandle = getHandleForInfo(rank, comm, &handle);

    // Special for CommTrack: we have different handles and infos for World and the self comms
    if (!hasHandle) {
        // Self
        if (comm == mySelfCommInfos[rank]) {
            hasHandle = true;
            handle = myCommSelfHandles[rank];
        }

        // World
        if (comm == myWorldCommInfos[rank]) {
            hasHandle = true;
            handle = myCommWorldHandles[rank];
        }
    }

    return passCommAcrossInternal(rank, comm, toPlaceId, pOutRemoteId, hasHandle, handle);
}

//=============================
// passCommAcrossInternal
//=============================
bool CommTrack::passCommAcrossInternal(
    int rank,
    Comm* comm,
    int toPlaceId,
    MustRemoteIdType* pOutRemoteId,
    bool hasHandle,
    MustCommType handle)
{
    // Do we have wrap-across at all?
    if (!myPassCommAcrossFunc)
        return false;

    // Valid info?
    if (!comm)
        return false;

    // Store the remote id
    if (pOutRemoteId)
        *pOutRemoteId = comm->getRemoteId();

    // Did we already pass this comm?
    if (comm->wasForwardedToPlace(toPlaceId, rank))
        return true;

    // Pass base resources of the comm
    if (!comm->isNull() && !comm->isPredefined())
        myLIdMod->passLocationToPlace(comm->myCreationPId, comm->myCreationLId, toPlaceId);

    MustRemoteIdType groupTableId = 0, remoteGroupTableId = 0;

    if (comm->myGroup)
        myGroupMod->passGroupTableAcross(rank, comm->myGroup, toPlaceId, &groupTableId);

    if (comm->myRemoteGroup)
        myGroupMod->passGroupTableAcross(rank, comm->myRemoteGroup, toPlaceId, &remoteGroupTableId);

    // Special handling for some arguments
    int nedges = 0;
    if (comm->myIsGraph && comm->myIndices && comm->myEdges)
        nedges = comm->myIndices[comm->myNnodes - 1];

    int* tempPeriods = NULL;
    if (comm->myIsCartesian && comm->myPeriods && comm->myNdims) {
        tempPeriods = new int[comm->myNdims];
        for (int i = 0; i < comm->myNdims; i++)
            tempPeriods[i] = (int)comm->myPeriods[i];
    }

    // Pass the actuall comm across
    (*myPassCommAcrossFunc)(
        rank,
        (int)hasHandle,
        handle,
        comm->getRemoteId(),
        (int)comm->myIsNull,
        (int)comm->myIsPredefined,
        (int)comm->myPredefined,
        (int)comm->myIsCartesian,
        (int)comm->myIsGraph,
        (int)comm->myIsIntercomm,
        comm->myContextId,
        groupTableId,
        remoteGroupTableId,
        comm->myCreationPId,
        comm->myCreationLId,
        comm->myReorder,
        comm->myNdims,
        comm->myDims,
        tempPeriods,
        comm->myNnodes,
        nedges,
        comm->myIndices,
        comm->myEdges,
        toPlaceId);

    // Tell the comm that we passed it across
    comm->setForwardedToPlace(toPlaceId, rank, myFreeCommAcrossFunc);

    // Free temp memory
    if (tempPeriods)
        delete[] tempPeriods;

    return true;
}

//=============================
// addPredefineds
//=============================
GTI_ANALYSIS_RETURN CommTrack::addPredefinedComms(
    MustParallelId pId,
    int reachableBegin,
    int reachableEnd,
    int worldSize,
    MustCommType commNull,
    MustCommType commSelf,
    MustCommType commWorld,
    int numWorlds,
    MustCommType* worlds,
    int numSelfs,
    MustCommType* selfs,
    I_ChannelId* channId)
{
    ////DEBUG
    // must::cout << "CommTrack: " << reachableBegin << "-" << reachableEnd << " size=" <<
    // worldSize; if (channId) 	must::cout << " id=" << channId->toString(); must::cout <<
    // std::endl;
    ////END DEBUG

    // Adept reachable interval
    if (myReachableBegin < 0) {
        getReachableRanks(&myReachableBegin, &myReachableEnd, reachableBegin);
        myReachableAvailable = true;
    }

    // Gather the different values for MPI_COMM_WORLD
    for (int i = reachableBegin; i < reachableBegin + numWorlds; i++) {
        myCommWorldHandles.insert(std::make_pair(i, worlds[i - reachableBegin]));
    }

    // Gather the different values for MPI_COMM_WORLD
    for (int i = reachableBegin; i < reachableBegin + numSelfs; i++) {
        myCommSelfHandles.insert(std::make_pair(i, selfs[i - reachableBegin]));
    }

#ifdef MUST_DEBUG
    must::cout << "CommTrack: addPredefineds, MPI_COMM_WORLD values: ";
    std::map<int, MustCommType>::iterator iter;
    for (iter = myCommWorldHandles.begin(); iter != myCommWorldHandles.end(); iter++) {
        if (iter != myCommWorldHandles.begin())
            must::cout << ", ";
        must::cout << iter->first << "->" << iter->second;
    }
    must::cout << std::endl;
#endif

    // Create information for MPI_COMM_WORLD
    // Store the real MPI_COMM_WORLD constant
    myRealCommWorld = commWorld;

    /*
     * We need one full info for each rank to make
     * their context ids independent!
     */
    if (static_cast<ssize_t>(myWorldCommInfos.size()) != worldSize)
        myWorldCommInfos.resize(worldSize);

    for (int i = reachableBegin; i < reachableBegin + numWorlds; i++) {
        myWorldCommInfos[i] = new Comm(this);
        myWorldCommInfos[i]->myIsNull = false;
        myWorldCommInfos[i]->myIsPredefined = true;
        myWorldCommInfos[i]->myGroup = myGroupMod->getGroupTable(
            0,
            worldSize - 1); // Interval group with ranks 0-(worldSize-1) were no mapping is needed;
        myWorldCommInfos[i]->myPredefinedName = "MPI_COMM_WORLD";
        myWorldCommInfos[i]->myPredefined = MUST_MPI_COMM_WORLD;
    }

    // Skip creation of self and null comms if already done
    if (mySelfCommInfos.size())
        return GTI_ANALYSIS_SUCCESS;

    // CommNull Information
    myNullInfo = new Comm(this);
    myNullInfo->myIsNull = true;

    // Create information for all the MPI_COMM_SELF's (one for each rank)
    /*
     * @todo this might be extended to only allocate them once they are actually needed. This
     *            should save a lot of memory and overhead at scale.
     */
    mySelfCommInfos.resize(worldSize);

    for (int i = 0; i < worldSize; i++) {
        mySelfCommInfos[i] = new Comm(this);
        mySelfCommInfos[i]->myIsPredefined = true;
        mySelfCommInfos[i]->myIsNull = false;
        mySelfCommInfos[i]->myGroup = myGroupMod->getGroupTable(i, i);
        mySelfCommInfos[i]->myPredefinedName = "MPI_COMM_SELF";
        mySelfCommInfos[i]->myPredefined = MUST_MPI_COMM_SELF;
    }

    // Call addPredefineds of TrackBase
    int predefEnumIds[2] = {(int)MUST_MPI_COMM_SELF, (int)MUST_MPI_COMM_WORLD};
    MustCommType predefValues[2] = {commSelf, commWorld};
    TrackBase<Comm, I_Comm, MustCommType, MustMpiCommPredefined, CommTrack, I_CommTrack>::
        addPredefineds(pId, commNull, 2, predefEnumIds, predefValues);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// isRankReachable
//=============================
bool CommTrack::isRankReachable(const I_Comm* comm, int rank)
{
    if (myReachableBegin < 0) {
        int rank, size;
        int err;
        PNMPI_modHandle_t handle;
        PNMPI_Service_descriptor_t service;

        // We need to check whether MPI_COMM_WORLD was splited
        /**
         * Reenabled this one:
         * We do this in MPI_Init, my PnMPI versions have recursion guards in MPI_Init
         * to ensure that we do not wrap MPI calls that an MPI implementation may call
         * before it issues MPI_Init (as pre of its MPI_Init implementation).
         * As a result, my PnMPI opts to not do any stacking for the
         * MPI_Comm_rank/size calls below, thus returning broken stuff.
         */
#ifdef PNMPI_FIXED
        err = PNMPI_Service_GetModuleByName("split_processes", &handle);
#else
        char string[512];
        sprintf(string, "%s", "split_processes");
        err = PNMPI_Service_GetModuleByName(string, &handle);
#endif
        if (err == PNMPI_SUCCESS) {
            MPI_Comm thisSetComm;

            err = PNMPI_Service_GetServiceByName(handle, "SplitMod_getMySetComm", "p", &service);
            assert(err == PNMPI_SUCCESS);
            ((int (*)(void*))service.fct)(&thisSetComm);

            PMPI_Comm_rank(thisSetComm, &rank);
            PMPI_Comm_size(thisSetComm, &size);
        } else {
            // No splitting is active, use MPI_COMM_WORLD
            PMPI_Comm_rank(MPI_COMM_WORLD, &rank);
            PMPI_Comm_size(MPI_COMM_WORLD, &size);
        }
        getReachableRanksForOwnId(&myReachableBegin, &myReachableEnd, rank);
        myReachableAvailable = true;
    }
    I_GroupTable* temp;
    if (!comm->isIntercomm())
        temp = comm->getGroup();
    else
        temp = comm->getRemoteGroup();

    // Translate
    int worldRank;
    if (!temp->translate(rank, &worldRank))
        return false;

    // Reachable ?
    if (worldRank < myReachableBegin || worldRank > myReachableEnd)
        return false;
    return true;
}

//=============================
// getComm
//=============================
I_Comm* CommTrack::getComm(MustParallelId pId, MustCommType comm)
{
    return getComm(pId2Rank(pId), comm);
}

//=============================
// getComm
//=============================
I_Comm* CommTrack::getComm(int rank, MustCommType comm) { return getCommInfo(rank, comm); }

//=============================
// getPersistentComm
//=============================
I_CommPersistent* CommTrack::getPersistentComm(MustParallelId pId, MustCommType comm)
{
    return getPersistentComm(pId2Rank(pId), comm);
}

//=============================
// getPersistentComm
//=============================
I_CommPersistent* CommTrack::getPersistentComm(int rank, MustCommType comm)
{
    Comm* ret = getCommInfo(rank, comm);
    ;
    if (ret)
        ret->incRefCount();
    return ret;
}

//=============================
// getRemoteComm
//=============================
I_Comm* CommTrack::getRemoteComm(MustParallelId pId, MustRemoteIdType remoteId)
{
    return getRemoteComm(pId2Rank(pId), remoteId);
}

//=============================
// getRemoteComm
//=============================
I_Comm* CommTrack::getRemoteComm(int rank, MustRemoteIdType remoteId)
{
    Comm* ret = getRemoteIdInfo(rank, remoteId);
    return ret;
}

//=============================
// getPersistentRemoteComm
//=============================
I_CommPersistent* CommTrack::getPersistentRemoteComm(MustParallelId pId, MustRemoteIdType remoteId)
{
    return getPersistentRemoteComm(pId2Rank(pId), remoteId);
}

//=============================
// getPersistentRemoteComm
//=============================
I_CommPersistent* CommTrack::getPersistentRemoteComm(int rank, MustRemoteIdType remoteId)
{
    Comm* ret = getRemoteIdInfo(rank, remoteId);
    if (ret)
        ret->incRefCount();
    return ret;
}

//=============================
// getCommInfo
//=============================
Comm* CommTrack::getCommInfo(MustParallelId pId, MustCommType comm)
{
    return getCommInfo(pId2Rank(pId), comm);
}

//=============================
// getCommInfo
//=============================
Comm* CommTrack::getCommInfo(int rank, MustCommType comm)
{

    // Is it a user defined type ?
    HandleMap::const_iterator pos = findUserHandle(rank, comm);

    if (pos == slock_safe_ptr(myUserHandles)->end()) {
        // If not a user comm-> it must be null or a predefined
        if (comm == myNullValue)
            return myNullInfo;

        // IMPORTANT MPI_COMM_WORLD can have different values on the processes
        // if we use the split module, So we have a special handling here
        // We compare both against the real MPI_COMM_WORLD and the rank
        // specific value that may differ due to the MPI split module!
        std::map<int, MustCommType>::iterator wHI = myCommWorldHandles.find(rank);
        if (wHI != myCommWorldHandles.end() && (wHI->second == comm || myRealCommWorld == comm)) {
            return myWorldCommInfos[rank];
        }
        wHI = myCommSelfHandles.find(rank);
        if (wHI != myCommSelfHandles.end() && (wHI->second == comm)) {
            return mySelfCommInfos[rank];
        }
    } else {
        return pos->second;
    }

    return NULL;
}

//=============================
// getWorldHandle
//=============================
MustCommType CommTrack::getWorldHandle(void) { return myRealCommWorld; }

//=============================
// notifyOfShutdown
//=============================
void CommTrack::notifyOfShutdown(void)
{
    TrackBase<Comm, I_Comm, MustCommType, MustMpiCommPredefined, CommTrack, I_CommTrack>::
        notifyOfShutdown();
    if (myGroupMod)
        myGroupMod->notifyOfShutdown();
}

/*EOF*/
