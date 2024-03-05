/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file AppThrAnn.cpp
 *       @see must::AppThrAnn.
 *
 *  @date 27.06.2017
 *  @author Simon Schwitanski
 */

#include "GtiMacros.h"
#include "AppThrAnn.h"
#include "MustEnums.h"
#include "MustDefines.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <sstream>
#include <fstream>

using namespace must;

mGET_INSTANCE_FUNCTION(AppThrAnn)
mFREE_INSTANCE_FUNCTION(AppThrAnn)
mPNMPI_REGISTRATIONPOINT_FUNCTION(AppThrAnn)

//=============================
// Constructor.
//=============================
AppThrAnn::AppThrAnn(const char* instanceName)
    : ModuleBase<AppThrAnn, I_AppThrAnn>(instanceName), myPropagateWinLock(0),
      myPropagateWinUnlock(0)
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUBMODULES 4
    if (subModInstances.size() < NUM_SUBMODULES) {
        std::cerr << "Module has not enough sub modules, check its analysis specification! ("
                  << __FILE__ << "@" << __LINE__ << ")" << std::endl;
        assert(0);
    }
    if (subModInstances.size() > NUM_SUBMODULES) {
        for (std::vector<I_Module*>::size_type i = NUM_SUBMODULES; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myPIdMod = (I_ParallelIdAnalysis*)subModInstances[0];
    myConsts = (I_BaseConstants*)subModInstances[1];
    myWinMod = (I_WinTrack*)subModInstances[2];
    myTSanMod = (I_TSan*)subModInstances[3];

    // Initialize module data
    // Nothing to do
    getWrapperFunction("propagateWinLock", (GTI_Fct_t*)&myPropagateWinLock);
    assert(myPropagateWinLock);
    getWrapperFunction("propagateWinUnlock", (GTI_Fct_t*)&myPropagateWinUnlock);
    assert(myPropagateWinUnlock);

    // Create data pool for annotation addresses
    myAnnPool = new DataPool<AnnData, 4>();
}

//=============================
// winLock
//=============================
GTI_ANALYSIS_RETURN
AppThrAnn::winLock(MustParallelId pId, MustLocationId lId, int lock_type, int rank, MustWinType win)
{
    int realRank = translateRank(myWinMod->getWin(pId, win)->getComm(), rank);

    AnnData* lock = NULL;
    // if this is a local win lock, acquire RW lock
    if (lock_type == MPI_LOCK_EXCLUSIVE && realRank == myPIdMod->getInfoForId(pId).rank) {
        std::map<MustWinType, AnnData*>::iterator it;
        it = myRWLockMap.find(win);
        if (it != myRWLockMap.end()) {
            lock = it->second;
        } else {
            lock = new AnnData();
            myRWLockMap.insert(std::make_pair(win, lock));
        }

        myTSanMod->annotateRWLockAcquired(pId, lId, lock, true);
    }

    if (myPropagateWinLock)
        myPropagateWinLock(pId, lId, lock_type, rank, win, lock);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// winUnlock
//=============================
GTI_ANALYSIS_RETURN
AppThrAnn::winUnlock(MustParallelId pId, MustLocationId lId, int rank, MustWinType win)
{

    AnnData* lock = NULL;
    // if this is a local win lock, release RW lock
    if (myRWLockMap.find(win) != myRWLockMap.end() && rank == myPIdMod->getInfoForId(pId).rank) {
        lock = myRWLockMap[win];
        myTSanMod->annotateRWLockReleased(pId, lId, lock, true);
    }

    if (myPropagateWinUnlock)
        myPropagateWinUnlock(pId, lId, rank, win, lock);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// annotateHappensBefore
//=============================
GTI_ANALYSIS_RETURN AppThrAnn::annotateHappensBefore(void* ann)
{
    myTSanMod->annotateHappensBefore(0, 0, ann);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// translateRank
//=============================
int AppThrAnn::translateRank(I_Comm* comm, int rank)
{
    int ret;
    if (rank != myConsts->getAnySource()) {
        if (!comm->isIntercomm())
            comm->getGroup()->translate(rank, &ret);
        else
            comm->getRemoteGroup()->translate(rank, &ret);
    } else {
        ret = rank;
    }

    return ret;
}

//=============================
// Destructor.
//=============================
AppThrAnn::~AppThrAnn(void)
{
    // clean up lock map
    for (std::map<MustWinType, AnnData*>::iterator it = myRWLockMap.begin();
         it != myRWLockMap.end();
         ++it)
        delete it->second;
    myRWLockMap.clear();

    if (myTSanMod)
        destroySubModuleInstance((I_Module*)myTSanMod);
    myTSanMod = NULL;
}
