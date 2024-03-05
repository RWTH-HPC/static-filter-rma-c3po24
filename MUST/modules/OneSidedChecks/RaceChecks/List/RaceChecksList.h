/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file RaceChecksList.h
 *       @see must::RaceChecksList.
 *
 *  @date 28.05.2023
 *  @author Simon Schwitanski
 *  @author Sem Klauke
 */

#include "ModuleBase.h"
#include "I_ParallelIdAnalysis.h"
#include "I_BaseConstants.h"
#include "I_CreateMessage.h"
#include "I_DatatypeTrack.h"
#include "I_RequestTrack.h"
#include "I_WinTrack.h"
#include "I_TSan.h"
#include "I_LocationAnalysis.h"
#include "I_RMATrack.h"
#include "I_VectorClock.h"
#include "I_TSanSyncClockRecorder.h"
#include "IntervalSkipList/IntervalSkiplist.h"
#include "I_GenerateLocationId.h"
#include "BaseApi.h"

#include "I_OriginRMAOp.h"
#include "I_RaceChecksList.h"

#include "RMATrack.h"

#include <map>
#include <unordered_map>
#include <vector>

#ifndef RACECHECKSLIST_H
#define RACECHECKSLIST_H

using namespace gti;

namespace must
{
/**
 * Correctness checks for memory operations at the origin.
 */
class RaceChecksList : public gti::ModuleBase<RaceChecksList, I_RaceChecksList>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    RaceChecksList(const char* instanceName);

    /**
     * @see I_RaceChecksList::originOpStart
     */
    GTI_ANALYSIS_RETURN originOpStart(MustRMAId rmaId);

    /**
     * @see I_RaceChecksList::originOpComplete
     */
    GTI_ANALYSIS_RETURN
    originOpComplete(MustParallelId pId, MustLocationId lId, MustRMAId* rmaId, int rmaIdLen);

    /**
     * Destructor.
     */
    virtual ~RaceChecksList(void);

    /**
     * @see I_RaceChecksList::targetOpStart
     */
    GTI_ANALYSIS_RETURN targetOpStart(MustRMAId rmaId);

    /**
     * @see I_RaceChecksList::targetOpComplete
     */
    GTI_ANALYSIS_RETURN
    targetOpComplete(MustParallelId pId, MustLocationId lId, MustRMAId* rmaId, int rmaIdLen);

    /**
     * @see I_RaceChecksList::winCreate
     */
    GTI_ANALYSIS_RETURN
    winCreate(MustWinType win, void* ann);

    /**
     * @see I_RaceChecksList::tsanAccess
     */
    GTI_ANALYSIS_RETURN
    tsanAccess(MustParallelId pId, void* pc, int8_t isRead, void* addr, int64_t count);

    /**
     * @see I_RaceChecksList::tsanAccessBulk
     */
    GTI_ANALYSIS_RETURN
    tsanAccessBulk(
        MustParallelId pId,
        void** readPc,
        size_t* readPcNum,
        void** readStartAddr,
        void** readEndAddr,
        size_t readLen,
        size_t readPcLen,
        void** writePc,
        size_t* writePcNum,
        void** writeStartAddr,
        void** writeEndAddr,
        size_t writeLen,
        size_t writePcLen);

    /**
     * @see I_RaceChecksList::winLock
     */
    GTI_ANALYSIS_RETURN winLock(
        MustParallelId pId,
        MustLocationId lId,
        int lock_type,
        int rank,
        MustWinType win,
        void* ann);

    /**
     * @see I_RaceChecksList::winFence
     */
    GTI_ANALYSIS_RETURN
    winFence(MustParallelId pId, MustLocationId lId, int assert, MustWinType win);

  protected:
    I_ParallelIdAnalysis* myPIdMod;
    I_CreateMessage* myLogger;
    I_BaseConstants* myConsts;
    I_DatatypeTrack* myDatMod;
    I_RequestTrack* myReqMod;
    I_LocationAnalysis* myLIdMod;
    I_WinTrack* myWinMod;
    I_RMATrack* myRMAMod;
    I_VectorClock* myVCMod;
    I_GenerateLocationId* myGenLId;
    I_TSanSyncClockRecorder* myTSanSyncClockRecorder;

  private:
    // store starting vc for each rma op
    std::unordered_map<MustRMAId, Clock> opClocks;
    // tmp. store MemAccess created at targetOpStart
    std::multimap<MustRMAId, ISL::MemAccess*> incompletedTargetAccess;
    // interval data structure for race check
    ISL::IntervalSkiplist* skiplist;
    // vc of the last window creation
    Clock winCreateClock;
    // discard data for race detection at MPI_Win_fence.
    // set by env. MUST_RESET_AT_FENCE
    bool resetAtFence = true;
    // for getLocationId
    handleNewLocationP myNewLocFunc;

    /**
     * Report race to MUST with 2 references
     *
     * @param pId1 parallel id of reference 1
     * @param lId1 location id of reference 1
     * @param pId2 parallel id of reference 2
     * @param lId2 location id of reference 2
     * @param msg additional message to add to MUST race report
     */
    void reportRace(
        MustParallelId pId1,
        MustLocationId lId1,
        MustParallelId pId2,
        MustLocationId lId2,
        std::string msg);
    /**
     * Fetch MustLocationId and store in MemAccess
     *
     * @param access mem access to fetch location id for
     */
    void fetchLocationIdForLocalAccess(ISL::MemAccess* access);

    /**
     * Extrat Basetype as MustMpiDatatypePredefined from Datatype
     *
     * @param datatype datatype from RMAOp
     * @return MustMpiDatatypePredefined enum
     */
    MustMpiDatatypePredefined extractBasetype(I_Datatype* datatype);

    /**
     * Translates a rank in a given communicator to the
     * corresponding rank in MPI_COMM_WORLD.
     *
     * @param comm communicator the rank belongs to
     * @param rank rank to convert
     * @return corresponding rank in MPI_COMM_WORLD
     */
    int translateRank(I_Comm* comm, int rank);

    /**
     * Get MustLocationid for given program counter
     *
     * @param pId parallel id of context we want the code location from
     * @param pc program counter we want to translate to location id
     * @return MustLocationId of the given program counter in pId context
     */
    MustLocationId getLocationId(MustParallelId pId, const void* pc) const;

}; /*class RaceChecksList.h*/
} /*namespace must*/

#endif /*RACECHECKSLIST_H*/
