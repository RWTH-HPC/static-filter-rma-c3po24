/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TargetChecks.h
 *       @see must::TargetChecks.
 *
 *  @date 13.06.2017
 *  @author Simon Schwitanski
 */

#include "ModuleBase.h"
#include "I_ParallelIdAnalysis.h"
#include "I_BaseConstants.h"
#include "I_CreateMessage.h"
#include "I_DatatypeTrack.h"
#include "I_RequestTrack.h"
#include "I_WinTrack.h"
#include "I_TSan.h"
#include "I_TSanSyncClockRecorder.h"
#include "I_LocationAnalysis.h"
#include "I_RMATrack.h"
#include "I_VectorClock.h"

#include "I_TargetRMAOp.h"
#include "I_TargetChecks.h"

#include <map>

#ifndef TARGETCHECKS_H
#define TARGETCHECKS_H

using namespace gti;

namespace must
{
/**
 * Correctness checks for memory operations at the target.
 */
class TargetChecks : public gti::ModuleBase<TargetChecks, I_TargetChecks>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    TargetChecks(const char* instanceName);

    /**
     * @see I_TargetChecks::targetOpStart
     */
    GTI_ANALYSIS_RETURN targetOpStart(MustRMAId rmaId);

    /**
     * @see I_TargetChecks::targetOpComplete
     */
    GTI_ANALYSIS_RETURN
    targetOpComplete(MustParallelId pId, MustLocationId lId, MustRMAId* rmaId, int rmaIdLen);

    /**
     * @see I_TargetChecks::winCreate
     */
    GTI_ANALYSIS_RETURN
    winCreate(MustWinType win, void* ann);

    /**
     * @see I_TargetChecks::winLock
     */
    GTI_ANALYSIS_RETURN winLock(
        MustParallelId pId,
        MustLocationId lId,
        int lock_type,
        int rank,
        MustWinType win,
        void* ann);

    /**
     * Destructor.
     */
    virtual ~TargetChecks(void);

  protected:
    I_ParallelIdAnalysis* myPIdMod;
    I_CreateMessage* myLogger;
    I_BaseConstants* myConsts;
    I_DatatypeTrack* myDatMod;
    I_RequestTrack* myReqMod;
    I_LocationAnalysis* myLIdMod;
    I_WinTrack* myWinMod;
    I_TSan* myTSanMod;
    I_RMATrack* myRMAMod;
    I_TSanSyncClockRecorder* myTSanSyncClockRecorder;
    I_VectorClock* myVCMod;

  private:
    void annotateMemAccess(
        MustParallelId pId,
        MustLocationId lId,
        const MustMemIntervalListType& memIntervals,
        bool isStore,
        const void* returnAddr,
        const void* funcAddr);

    void annotateAtomicAccesses(
        MustParallelId pId,
        MustLocationId lId,
        bool isStore,
        const MustMemIntervalListType& memIntervals,
        MustMpiDatatypePredefined baseType);

    void annotateFuncEntry(MustParallelId pId, MustParallelId lId);

    void annotateFuncExit();

    MustMpiDatatypePredefined extractBasetype(I_Datatype* datatype);

    std::map<int, void*> myFiberPool;

    // store TSan sync clocks for *local* window lock calls in Tsan,
    // because it could be the begin of the concurrent region
    std::map<MustWinType, const void*> myWinLockAddrs;

    // store TSan sync clocks for window creation calls,
    // because it could be the begin of the concurrent region
    // if taget and origin did not synchronize no far
    std::map<MustWinType, void*> myWinCreateAddrs;

    /**
     * Translates a rank in a given communicator to the
     * corresponding rank in MPI_COMM_WORLD.
     *
     * @param comm communicator the rank belongs to
     * @param rank rank to convert
     * @return corresponding rank in MPI_COMM_WORLD
     */
    int translateRank(I_Comm* comm, int rank);

    // Stores whether TSanMessages module has been loaded.
    // If so, we add further meta information to TSan annotations
    // to parse them in the TSanMessages module.
    bool myHasTSanMessages;

}; /*class TargetChecks.h*/
} /*namespace must*/

#endif /*TARGETCHECKS.H_H*/
