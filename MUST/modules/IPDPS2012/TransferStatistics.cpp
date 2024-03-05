/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TransferStatistics.cpp
 *       @see MUST::TransferStatistics.
 *
 *  @date 16.09.2011
 *  @author Tobias Hilbrich, Mathias Korepkat, Joachim Protze
 */

#include "GtiMacros.h"
#include "MustEnums.h"
#include "PrefixedOstream.hpp"

#include "TransferStatistics.h"

using namespace must;

mGET_INSTANCE_FUNCTION(TransferStatistics)
mFREE_INSTANCE_FUNCTION(TransferStatistics)
mPNMPI_REGISTRATIONPOINT_FUNCTION(TransferStatistics)

//=============================
// Constructor
//=============================
TransferStatistics::TransferStatistics(const char* instanceName)
    : gti::ModuleBase<TransferStatistics, I_TransferStatistics>(instanceName), myP2PSent(0),
      myNumP2POps(0), myCollSent(0), myNumCollOps(0), myIntervalStart(), myWorldRank(-1),
      myWorldSize(-1)
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUBS 3
    if (subModInstances.size() < NUM_SUBS) {
        must::cerr << "Module has not enough sub modules, check its analysis specification! ("
                   << __FILE__ << "@" << __LINE__ << ")" << std::endl;
        assert(0);
    }
    if (subModInstances.size() > NUM_SUBS) {
        for (std::vector<I_Module*>::size_type i = NUM_SUBS; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myPIdMod = (I_ParallelIdAnalysis*)subModInstances[0];
    myCommTrack = (I_CommTrack*)subModInstances[1];
    myTypeTrack = (I_DatatypeTrack*)subModInstances[2];
    // myCollCond = (I_CollectiveCondition*) subModInstances[3];

    // Initialize module data
    getWrapperFunction("statisticsInterval", (GTI_Fct_t*)&myFP);
}

//=============================
// Destructor
//=============================
TransferStatistics::~TransferStatistics()
{
    if (myPIdMod)
        destroySubModuleInstance((I_Module*)myPIdMod);
    myPIdMod = NULL;

    if (myCommTrack)
        destroySubModuleInstance((I_Module*)myCommTrack);
    myCommTrack = NULL;

    if (myTypeTrack)
        destroySubModuleInstance((I_Module*)myTypeTrack);
    myTypeTrack = NULL;

    /*	if (myCollCond)
                destroySubModuleInstance ((I_Module*) myCollCond);
            myCollCond = NULL;*/
}

//=============================
// init
//=============================
GTI_ANALYSIS_RETURN TransferStatistics::init(MustParallelId pId)
{
    myP2PSent = 0;
    myNumP2POps = 0;
    myCollSent = 0;
    myNumCollOps = 0;

    myWorldRank = myPIdMod->getInfoForId(pId).rank;

    if (myWorldRank == 0) {
        gettimeofday(&myIntervalStart, NULL);
    }

    myWorldSize = -1; // we initialize this later on

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// sendP2P
//=============================
GTI_ANALYSIS_RETURN
TransferStatistics::sendP2P(MustParallelId pId, MustDatatypeType type, int count)
{
    I_Datatype* tInfo = myTypeTrack->getDatatype(pId, type);

    if (!tInfo || tInfo->isNull())
        return GTI_ANALYSIS_SUCCESS;

    myNumP2POps++;
    myP2PSent += tInfo->getSize() * count;

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// collNoTransfer
//=============================
GTI_ANALYSIS_RETURN TransferStatistics::collNoTransfer(MustParallelId pId, MustCommType comm)
{
    // Check whether this is a global coll; complete the intervall in that case
    if (!checkAndCompleteIntervall(pId, comm))
        return GTI_ANALYSIS_SUCCESS;

    // Add this op to the interval
    myNumCollOps++;

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// collSend
//=============================
GTI_ANALYSIS_RETURN TransferStatistics::collSend(
    MustParallelId pId,
    MustCommType comm,
    MustDatatypeType type,
    int count)
{
    // Check whether this is a global coll; complete the intervall in that case
    if (!checkAndCompleteIntervall(pId, comm))
        return GTI_ANALYSIS_SUCCESS;

    // Add this op to the interval
    I_Datatype* tInfo = myTypeTrack->getDatatype(pId, type);
    if (!tInfo || tInfo->isNull())
        return GTI_ANALYSIS_SUCCESS;

    myNumCollOps++;
    myCollSent += tInfo->getSize() * count;

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// collSendN
//=============================
GTI_ANALYSIS_RETURN TransferStatistics::collSendN(
    MustParallelId pId,
    MustCommType comm,
    MustDatatypeType type,
    int count)
{
    // Check whether this is a global coll; complete the intervall in that case
    if (!checkAndCompleteIntervall(pId, comm))
        return GTI_ANALYSIS_SUCCESS;

    // Add this op to the interval
    I_Datatype* tInfo = myTypeTrack->getDatatype(pId, type);
    if (!tInfo || tInfo->isNull())
        return GTI_ANALYSIS_SUCCESS;
    I_Comm* cInfo = myCommTrack->getComm(pId, comm);

    myNumCollOps++;
    myCollSent += tInfo->getSize() * count * cInfo->getGroup()->getSize();

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// collSendCounts
//=============================
GTI_ANALYSIS_RETURN TransferStatistics::collSendCounts(
    MustParallelId pId,
    MustCommType comm,
    MustDatatypeType type,
    int* counts)
{
    // Check whether this is a global coll; complete the intervall in that case
    if (!checkAndCompleteIntervall(pId, comm))
        return GTI_ANALYSIS_SUCCESS;

    // Add this op to the interval
    I_Datatype* tInfo = myTypeTrack->getDatatype(pId, type);
    if (!tInfo || tInfo->isNull())
        return GTI_ANALYSIS_SUCCESS;
    I_Comm* cInfo = myCommTrack->getComm(pId, comm);

    myNumCollOps++;

    uint64_t tSize = tInfo->getSize();
    for (int i = 0; i < cInfo->getGroup()->getSize(); i++) {
        myCollSent += tSize * counts[i];
    }

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// collSendTypes
//=============================
GTI_ANALYSIS_RETURN TransferStatistics::collSendTypes(
    MustParallelId pId,
    MustCommType comm,
    MustDatatypeType* types,
    int* counts)
{
    // Check whether this is a global coll; complete the intervall in that case
    if (!checkAndCompleteIntervall(pId, comm))
        return GTI_ANALYSIS_SUCCESS;

    // Add this op to the interval
    I_Comm* cInfo = myCommTrack->getComm(pId, comm);

    myNumCollOps++;

    for (int i = 0; i < cInfo->getGroup()->getSize(); i++) {
        I_Datatype* tInfo = myTypeTrack->getDatatype(pId, types[i]);
        if (!tInfo || tInfo->isNull())
            continue;

        myCollSent += tInfo->getSize() * counts[i];
    }

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// checkAndCompleteIntervall
//=============================
bool TransferStatistics::checkAndCompleteIntervall(MustParallelId pId, MustCommType comm)
{
    I_Comm* cInfo = myCommTrack->getComm(pId, comm);

    if (!cInfo || cInfo->isNull())
        return false;

    if (myWorldSize < 0)
        myWorldSize =
            myCommTrack->getComm(pId, myCommTrack->getWorldHandle())->getGroup()->getSize();

    // Is it a globally synchronizing collective ?
    if (cInfo->getGroup()->getSize() == myWorldSize) {
        uint64_t time = 0;

        if (myWorldRank == 0) {
            struct timeval current;
            gettimeofday(&current, NULL);

            time = (uint64_t)current.tv_sec * (uint64_t)1000000 + (uint64_t)current.tv_usec -
                   ((uint64_t)myIntervalStart.tv_sec * (uint64_t)1000000 +
                    (uint64_t)myIntervalStart.tv_usec);
            myIntervalStart = current;
        }

        // Create record for current interval
        if (myFP)
            (*myFP)(myP2PSent, myNumP2POps, myCollSent, myNumCollOps, time);

        myP2PSent = 0;
        myNumP2POps = 0;
        myCollSent = 0;
        myNumCollOps = 0;
    }

    return true;
}

//=============================
// collRecvBalance
//=============================
GTI_ANALYSIS_RETURN
TransferStatistics::collRecvBalance(MustParallelId pId, MustCommType comm, int coll, int root)
{
    if ((MustCollCommType)coll == MUST_COLL_BCAST)
        return collNoTransfer(pId, comm);

    if ((MustCollCommType)coll == MUST_COLL_SCATTER ||
        (MustCollCommType)coll == MUST_COLL_SCATTERV) {
        if (myPIdMod->getInfoForId(pId).rank != root)
            return collNoTransfer(pId, comm);
    }

    return GTI_ANALYSIS_SUCCESS;
}

/*EOF*/
