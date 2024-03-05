/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file RaceChecksList.cpp
 *       @see must::RaceChecksList.
 *
 *  @date 28.05.2023
 *  @author Simon Schwitanski
 *  @author Sem Klauke
 */

#include "GtiMacros.h"
#include "RaceChecksList.h"
#include "MustEnums.h"
#include "MustDefines.h"
#include <cstdlib>
#include <iostream>
#include <dlfcn.h>
#include <link.h>
#include <dlfcn.h>

#include <sstream>

using namespace must;
using namespace ISL;

/* debugging output for local, target, origin ops */
// #define MUST_DEBUG 1
// #define MUST_DEBUG_LOCAL 2
// #define MUST_DEBUG_TARGET 1
// #define MUST_DEBUG_ORIGIN 1

mGET_INSTANCE_FUNCTION(RaceChecksList)
mFREE_INSTANCE_FUNCTION(RaceChecksList)
mPNMPI_REGISTRATIONPOINT_FUNCTION(RaceChecksList)

//=============================
// Constructor.
//=============================
RaceChecksList::RaceChecksList(const char* instanceName)
    : ModuleBase<RaceChecksList, I_RaceChecksList>(instanceName)
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUBMODULES 11
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
    myLogger = (I_CreateMessage*)subModInstances[1];
    myConsts = (I_BaseConstants*)subModInstances[2];
    myDatMod = (I_DatatypeTrack*)subModInstances[3];
    myReqMod = (I_RequestTrack*)subModInstances[4];
    myLIdMod = (I_LocationAnalysis*)subModInstances[5];
    myWinMod = (I_WinTrack*)subModInstances[6];
    myRMAMod = (I_RMATrack*)subModInstances[7];
    myTSanSyncClockRecorder = (I_TSanSyncClockRecorder*)subModInstances[8];
    myVCMod = (I_VectorClock*)subModInstances[9];
    myGenLId = (I_GenerateLocationId*)subModInstances[10];

    getWrapperFunction("handleNewLocation", (GTI_Fct_t*)&myNewLocFunc);

    // Initialize module data
    this->skiplist = new IntervalSkiplist{0.5};

    // read env varible to disable resetting at fence
    if (const char* env_fence = std::getenv("MUST_RESET_AT_FENCE")) {
        resetAtFence = (bool)atoi(env_fence);
    }
}

/**
 * @see I_RaceChecksList::originOpStart
 */
GTI_ANALYSIS_RETURN RaceChecksList::originOpStart(MustRMAId rmaId)
{
    I_OriginRMAOpPersistent* op = myRMAMod->getPersistentOriginRMAOp(rmaId);

#ifdef MUST_DEBUG_ORIGIN
    std::stringstream msg;
    msg << "OriginRMAOp STARTED: ";
    msg << "callId: " << rmaId;
    msg << ", pId: " << op->getPId();
    msg << ", lId: " << op->getLId();
    msg << ", isStore: " << op->isStore();
    msg << ", win: " << op->getWin();
    msg << ", request: " << op->getRequest();
    msg << ", target: " << op->getTarget();
    msg << ", current VC: " << myVCMod->getClock().toStr();
    msg << std::endl;
    std::cout << msg.str();
#endif

    // safe staring VC of this RMA Op
    opClocks.emplace(std::make_pair(rmaId, myVCMod->getClock()));

    return GTI_ANALYSIS_SUCCESS;
}

/**
 * @see I_RaceChecksList::originOpComplete
 */
GTI_ANALYSIS_RETURN RaceChecksList::originOpComplete(
    MustParallelId pId,
    MustLocationId lId,
    MustRMAId* rmaId,
    int rmaIdLen)
{
    if (rmaIdLen == 0)
        return GTI_ANALYSIS_SUCCESS;
#ifdef MUST_DEBUG_ORIGIN
    std::cout << "=== ORIGIN OPS COMPLETED ===" << std::endl;
#endif
    for (int i_op = rmaIdLen - 1; i_op >= 0; --i_op) {
        I_OriginRMAOpPersistent* op = myRMAMod->getPersistentOriginRMAOp((rmaId[i_op]));
        // get vector clocks of concurrent region
        Clock startConcurRegion = opClocks[rmaId[i_op]];
        Clock endConcurRegion = myVCMod->getClock();

#ifdef MUST_DEBUG_ORIGIN
        std::stringstream msg;
        msg << "OriginRMAOp COMPLETED: ";
        msg << "callId: " << rmaId[i_op];
        msg << ", pId: " << op->getPId();
        msg << ", lId: " << op->getLId();
        msg << ", isStore: " << op->isStore();
        msg << ", win: " << op->getWin();
        msg << ", request: " << op->getRequest();
        msg << ", target: " << op->getTarget();
        msg << ", current VC: " << myVCMod->getClock().toStr();
        msg << std::endl;
        std::cout << msg.str();
#endif

        // check each memory interval effected by this rma op
        for (auto& mem : op->getMemIntervals()) {
#ifdef MUST_DEBUG_ORIGIN
            std::stringstream msg;
            msg << "stride: " << mem.stride;
            msg << ", blocksize: " << mem.blocksize;
            msg << ", count: " << mem.count;
            msg << ", repetition: " << mem.repetition;
            msg << std::endl;
            std::cout << msg.str();
#endif
            // loop through the blocks of memory in this memory interval
            for (int i = 0; i < mem.count; ++i) {
                // start and end of this memory block
                MemAddress start = mem.baseAddress + (mem.stride * i);
                MemAddress end = start + mem.blocksize - 1;
#ifdef MUST_DEBUG_ORIGIN
                std::stringstream msg;
                msg << "Access: [" << (void*)start << "," << (void*)(end);
                msg << "] with concurrent region " << startConcurRegion.toStr();
                msg << " -- " << endConcurRegion.toStr() << std::endl;
                std::cout << msg.str();
#endif
                // extract datatype and size for alignment checks
                auto dt = extractBasetype(mem.type);
                auto dtSize = (mem.type != nullptr) ? mem.type->getSize() : 4;
                // create MemAccess to check with the skiplist and insert it
                MemAccess* ac = MemAccess::createOriginAccess(
                    op->isStore(),
                    rmaId[i_op],
                    op->getPId(),
                    op->getLId(),
                    start,
                    end,
                    dt,
                    dtSize);
                ac->insertVectorClock(startConcurRegion, endConcurRegion);
                // data race check with skiplist
                std::vector<MemAccess*> conflicts{};
                bool hasConflict = skiplist->insertAndCheckConflicts(ac, conflicts);
                if (hasConflict) {
#ifdef MUST_DEBUG_ORIGIN
                    std::cout << "Found " << conflicts.size() << " conflicts at origin"
                              << std::endl;
#endif
                    // report each access individually
                    for (auto access : conflicts) {
                        fetchLocationIdForLocalAccess(access);
                        reportRace(
                            op->getPId(),
                            op->getLId(),
                            access->pId,
                            access->lId,
                            "on origin");
                    }
                }
            }
        }
    }

    return GTI_ANALYSIS_SUCCESS;
}

void RaceChecksList::reportRace(
    MustParallelId pId1,
    MustLocationId lId1,
    MustParallelId pId2,
    MustLocationId lId2,
    std::string msg)
{
    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    refs.push_back(std::make_pair(pId1, lId1));
    refs.push_back(std::make_pair(pId2, lId2));
    std::stringstream stream;
    stream << "Found RMA data race (overlapping accesses) " << msg;
    myLogger
        ->createMessage(MUST_WARNING_DATARACE, pId1, lId1, MustErrorMessage, stream.str(), refs);
    // std::cout << "[MUST-SELF-REPORT] " << stream.str() << std::endl;
}

GTI_ANALYSIS_RETURN RaceChecksList::winCreate(MustWinType win, void* ann)
{
    /* If a process was never syncronised before the first (target) RMA operation,
       we tick at window creation and safe this as starting vector clock for the next,
       RMA op, since accesses to the window before that are safe */
    myVCMod->tick();
    winCreateClock = myVCMod->getClock();
    return GTI_ANALYSIS_SUCCESS;
}

GTI_ANALYSIS_RETURN RaceChecksList::winLock(
    MustParallelId pId,
    MustLocationId lId,
    int lock_type,
    int rank,
    MustWinType win,
    void* ann)
{
    return GTI_ANALYSIS_SUCCESS;
}

/**
 * @see I_RaceChecksList::targetOpStart
 */
GTI_ANALYSIS_RETURN RaceChecksList::targetOpStart(MustRMAId rmaId)
{
    I_TargetRMAOpPersistent* op = myRMAMod->getPersistentTargetRMAOp(rmaId);
    /* Get last signal vector clock for this rma op:
        - if originClock == -1 then origin == target, so we ignore
        - if originClock is still 0 there was never any sync between these two processes,
          so we take the windown creation vector clock as last signal, since access
          before that are safe (are they?)
        - otherwise just use vector clock from SyncClockRecorder
    */
    Clock lastSignal;
    int originClock = op->getClock();
    if (originClock == -1)
        lastSignal = myVCMod->getClock();
    else if (originClock == 0)
        lastSignal = winCreateClock;
    else
        lastSignal = myTSanSyncClockRecorder->getVCSyncClock(originClock);

#ifdef MUST_DEBUG_TARGET
    std::stringstream msg;
    msg << "TargetRMAOp STARTED: ";
    msg << "callId: " << rmaId;
    msg << ", pId: " << op->getPId();
    msg << ", lId: " << op->getLId();
    msg << ", isStore: " << op->isStore();
    msg << ", win: " << op->getWin();
    msg << ", target: " << op->getTarget();
    msg << ", originClock: " << op->getClock();
    msg << ", current VC: " << myVCMod->getClock().toStr();
    msg << ", using VC: " << lastSignal.toStr();
    msg << std::endl;
    std::cout << msg.str();
#endif

    for (auto& mem : op->getMemIntervals()) {
        for (int i = 0; i < mem.count; ++i) {
            MemAddress start = mem.baseAddress + (mem.stride * i);
            MemAddress end = start + mem.blocksize - 1;
#ifdef MUST_DEBUG_TARGET
            std::stringstream msg;
            msg << "Access: [" << (void*)start << "," << (void*)(end) << "] with last signal ";
            msg << lastSignal.toStr() << std::endl;
            std::cout << msg.str();
#endif
            // save memory access in MemAccess
            auto dt = extractBasetype(mem.type);
            auto dtSize = (mem.type != nullptr) ? mem.type->getSize() : 4;
            MemAccess* ac = MemAccess::createTargetAccess(
                op->isStore(),
                op->isAtomic(),
                rmaId,
                op->getPId(),
                op->getLId(),
                start,
                end,
                dt,
                dtSize);
            ac->insertVectorClock(lastSignal, lastSignal);
            // safe pointer for later to add end-vector-clock to it at targetOpComplete
            incompletedTargetAccess.emplace(std::make_pair(rmaId, ac));
            // data race check with skiplist
            std::vector<MemAccess*> conflicts{};
            bool hasConflict = skiplist->insertAndCheckConflicts(ac, conflicts);
            /* if the conflict is between RMA calls from the same origin we need to
             * check whether they are already consisten and therefore don't produce a race */
            if (hasConflict) {
                for (auto access : conflicts) {
                    // both are rma calls
                    if (access->hasConcurrentRegion()) {
                        I_TargetRMAOp* access_op = myRMAMod->getTargetRMAOp(access->rmaId);
                        // from the same origin rank
                        if (access_op != NULL && op->getOrigin() == access_op->getOrigin()) {
                            // get state of the other rma op
                            TargetOpState access_state = (TargetOpState)myRMAMod->getTargetOpState(
                                access_op->getWinId(),
                                access_op->getOrigin(),
                                access->rmaId);
                            // if that other op isn't consistent, we have a race
                            if (access_state == TARGET_OP_STATE_INIT) {
                                //fetchLocationIdForLocalAccess(access);
                                reportRace(
                                    op->getPId(),
                                    op->getLId(),
                                    access->pId,
                                    access->lId,
                                    "on target (start).");
                            }
                        }
                    }
                }
            }
        }
    }

    opClocks.emplace(std::make_pair(rmaId, lastSignal));

    return GTI_ANALYSIS_SUCCESS;
}

/**
 * @see I_RaceChecksList::targetOpComplete
 */
GTI_ANALYSIS_RETURN RaceChecksList::targetOpComplete(
    MustParallelId pId,
    MustLocationId lId,
    MustRMAId* rmaId,
    int rmaIdLen)
{
#ifdef MUST_DEBUG_TARGET
    std::cout << "=== TARGET OPS COMPLETED ===" << std::endl;
#endif

    for (int i_op = 0; i_op < rmaIdLen; ++i_op) {
        I_TargetRMAOpPersistent* op = myRMAMod->getPersistentTargetRMAOp(rmaId[i_op]);
        Clock endConcurRegion = myVCMod->getClock();

#ifdef MUST_DEBUG_TARGET
        std::stringstream msg;
        msg << "TargetRMAOp COMPLETED: ";
        msg << "callId: " << rmaId[i_op];
        msg << ", pId: " << op->getPId();
        msg << ", lId: " << op->getLId();
        msg << ", isStore: " << op->isStore();
        msg << ", isLocked: " << op->isLocked();
        msg << ", win: " << op->getWin();
        msg << ", win id: " << op->getWinId();
        msg << ", target: " << op->getTarget();
        msg << ", originClock: " << op->getClock(); // Last Singal send from the origin
        msg << ", current VC: " << endConcurRegion.toStr();
        msg << std::endl;
        std::cout << msg.str();
#endif
        // look for the completed target ops in the map from targetOpStarted
        auto incAccesses = incompletedTargetAccess.equal_range(rmaId[i_op]);
        for (auto i = incAccesses.first; i != incAccesses.second; ++i) {
            MemAccess* ac = i->second;
            // add end-vector-clock
            ac->vectorClock2 = endConcurRegion;
            // do race check again with new concurrent region
            std::vector<MemAccess*> conflicts = skiplist->findConflictingMemAccesses_not_const(*ac);
            for (auto access : conflicts) {
                /* but if it's an race between rma ops from the same origin,
                   we already covert that in targetOpStart */
                if (access->hasConcurrentRegion()) {
                    I_TargetRMAOp* access_op = myRMAMod->getTargetRMAOp(access->rmaId);
                    if (access_op != NULL && op->getOrigin() == access_op->getOrigin())
                        continue;
                }
                // report race
                fetchLocationIdForLocalAccess(access);
                reportRace(op->getPId(), op->getLId(), access->pId, access->lId, "on target (end).");
            }
        }
        incompletedTargetAccess.erase(rmaId[i_op]);
    }

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// extractBasetype
//=============================
MustMpiDatatypePredefined RaceChecksList::extractBasetype(I_Datatype* datatype)
{
    if (datatype == nullptr) {
#ifdef MUST_DEBUG
        std::cout << "I_Datatype was nullptr" << std::endl;
#endif
        return MustMpiDatatypePredefined::MUST_MPI_DATATYPE_UNKNOWN;
    }

    MustTypesigType typesig = datatype->getTypesig();

    // set type to first basetype in entry
    MustMpiDatatypePredefined type = typesig.front().second;

    for (MustTypesigType::iterator it = typesig.begin(); it != typesig.end(); ++it) {
        // check that all basetypes are the same
        if (type != it->second && type != MUST_MPI_LB && type != MUST_MPI_UB)
            return MUST_MPI_DATATYPE_UNKNOWN;
    }

    return type;
}

//=============================
// translateRank
//=============================
int RaceChecksList::translateRank(I_Comm* comm, int rank)
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
// getLocationId
//=============================
MustLocationId RaceChecksList::getLocationId(MustParallelId pId, const void* pc) const
{
    MustLocationId lId = myGenLId->getNextLocationId();
    Dl_info info, info2;
    struct link_map* link_map;
    size_t vmaOffset{0};

    if (dladdr(pc, &info) == 0) {
        return 0;
    }
    if (dladdr1((void*)pc, &info2, (void**)&link_map, RTLD_DL_LINKMAP) != 0)
        vmaOffset = link_map->l_addr;

#ifdef MUST_DEBUG
    printf("vmaOffset: %p addr: %p\n", (void*)vmaOffset, (void*)((uintptr_t)pc - vmaOffset));
#endif

    (*myNewLocFunc)(
        pId,
        lId,
        "\0",
        1,
        info.dli_fbase,
        (void*)((uintptr_t)pc - vmaOffset),
        info.dli_fname,
        strlen(info.dli_fname) + 1,
        info.dli_fbase
#ifdef ENABLE_STACKTRACE
        ,
        0,
        0,
        0,
        nullptr,
        nullptr
#endif
    );
    return lId;
}

/**
 * Passed from Tsan annotation
 * @see I_RaceChecksList::tsanAccess
 */
GTI_ANALYSIS_RETURN
RaceChecksList::tsanAccess(MustParallelId pId, void* pc, int8_t isRead, void* addr, int64_t count)
{
    MustAddressType left = reinterpret_cast<MustAddressType>(addr);
#ifdef MUST_DEBUG_LOCAL
    std::stringstream s;
    s << "LOCAL ACCESS [" << (void*)left << "," << (void*)(left + (count - 1)) << "] with clock ";
    s << myVCMod->getClock().toStr() << " and pc: " << pc << std::endl;
    std::cout << s.str();
#endif
    // create MemAccess with current VC
    MemAccess* ac = MemAccess::createLocalAccess(!isRead, pId, 0, left, left + (count - 1), pc);
    ac->insertVectorClock(myVCMod->getClock());
    std::vector<MemAccess*> conflicts{};
    // data race check with skiplist
    bool hasConflict = skiplist->insertAndCheckConflicts(ac, conflicts);

    if (hasConflict) {
#ifdef MUST_DEBUG_LOCAL
        std::cout << "Found " << conflicts.size() << " conflicts with local access" << std::endl;
#endif
        fetchLocationIdForLocalAccess(ac);
        // report each race individually
        for (auto access : conflicts) {
            fetchLocationIdForLocalAccess(access);
            reportRace(ac->lId, ac->lId, access->pId, access->lId, "with local store/load");
        }
    }
    return GTI_ANALYSIS_SUCCESS;
}

/**
 * Passed from Tsan annotation (as arrays, not one by one)
 * @see I_RaceChecksList::tsanAccessBulk
 */
GTI_ANALYSIS_RETURN RaceChecksList::tsanAccessBulk(
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
    size_t writePcLen)
{
#ifdef MUST_DEBUG_LOCAL
    std::stringstream s;
    s << "LOCAL ACCESS: " << readLen << " read, " << writeLen << " write";
    s << " with clock " << myVCMod->getClock().toStr() << std::endl;
    std::cout << s.str();
#endif

    Clock vc = myVCMod->getClock();
    std::vector<MemAccess*> conflicts{};

    // insert reads
    for (size_t r = 0, rPc = 0; r < readLen; ++r) {
#if MUST_DEBUG_LOCAL == 2
        std::stringstream s;
        s << "- READ [" << readStartAddr[r] << "," << readEndAddr[r];
        s << "] with clock " << vc.toStr() << " and last pc: " << readPc[rPc+readPcNum[r]-1] << std::endl;
        std::cout << s.str();
#endif
        /* readPc[rPc] ... readPc[rPc+readPcNum[r]-1] hold all program counter that
         * accessed this memory interval. We will store just the last one.*/
        MemAccess* ac = MemAccess::createLocalAccess(
            false,
            pId,
            0,
            (MustAddressType)readStartAddr[r],
            (MustAddressType)readEndAddr[r],
            readPc[rPc+readPcNum[r]-1]);
        ac->insertVectorClock(vc);
        if (skiplist->insertAndCheckConflicts(ac, conflicts)) {
            // handle conflicts that the skiplist found
#ifdef MUST_DEBUG_LOCAL
            std::cout << "Found " << conflicts.size() << " conflicts with local read" << std::endl;
#endif
            fetchLocationIdForLocalAccess(ac);
            // report each race individually
            for (auto access : conflicts) {
                fetchLocationIdForLocalAccess(access);
                reportRace(ac->lId, ac->lId, access->pId, access->lId, "with local load");
            }
        }
        rPc += readPcNum[r];
        conflicts.clear();
    }

    // insert wrirtes
    for (size_t w = 0, wPc = 0; w < writeLen; ++w) {
#if MUST_DEBUG_LOCAL == 2
        std::stringstream s;
        s << "- WRITE [" << writeStartAddr[w] << "," << writeEndAddr[w];
        s << "] with clock " << vc.toStr() << " and pc: " << writePc[wPc+writePcNum[w]-1] << std::endl;
        std::cout << s.str();
#endif
        MemAccess* ac = MemAccess::createLocalAccess(
            true,
            pId,
            0,
            (MustAddressType)writeStartAddr[w],
            (MustAddressType)writeEndAddr[w],
            writePc[wPc+writePcNum[w]-1]);
        ac->insertVectorClock(vc);
        if (skiplist->insertAndCheckConflicts(ac, conflicts)) {
            // handle conflicts that the skiplist found
#ifdef MUST_DEBUG_LOCAL
            std::cout << "Found " << conflicts.size() << " conflicts with local write" << std::endl;
#endif
            fetchLocationIdForLocalAccess(ac);
            // report each race individually
            for (auto access : conflicts) {
                fetchLocationIdForLocalAccess(access);
                reportRace(ac->lId, ac->lId, access->pId, access->lId, "with local store");
            }
        }
        wPc += writePcNum[w];
        
        conflicts.clear();
    }

    return GTI_ANALYSIS_SUCCESS;
}

/**
 * @see I_RaceChecksList::targetOpComplete
 */
GTI_ANALYSIS_RETURN
RaceChecksList::winFence(MustParallelId pId, MustLocationId lId, int assert, MustWinType win)
{
    /* At MPI_Win_fence() we can delete the already saves data */

    if (not resetAtFence)
        return GTI_ANALYSIS_SUCCESS;

    opClocks.clear();
    incompletedTargetAccess.clear();
    skiplist->clear();
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// fetchLocationIdForLocalAccess
//=============================
/* If the locationId for a MemAccess object isn't set, we do so.
 * This way getLocationId() is only called when needed but not twice for one MemAccess */
void RaceChecksList::fetchLocationIdForLocalAccess(ISL::MemAccess* access)
{
    if (access->lId == 0 && access->pc != nullptr) {
        // locationId isn't set
        access->lId = getLocationId(access->pId, access->pc);
    }
}

//=============================
// Destructor.
//=============================
RaceChecksList::~RaceChecksList(void)
{
    if (myPIdMod)
        destroySubModuleInstance((I_Module*)myPIdMod);
    myPIdMod = NULL;

    if (myLogger)
        destroySubModuleInstance((I_Module*)myLogger);
    myLogger = NULL;

    if (myConsts)
        destroySubModuleInstance((I_Module*)myConsts);
    myConsts = NULL;

    if (myDatMod)
        destroySubModuleInstance((I_Module*)myDatMod);
    myDatMod = NULL;

    if (myReqMod)
        destroySubModuleInstance((I_Module*)myReqMod);
    myReqMod = NULL;

    if (myWinMod)
        destroySubModuleInstance((I_Module*)myWinMod);
    myWinMod = NULL;

    if (myRMAMod)
        destroySubModuleInstance((I_Module*)myRMAMod);
    myRMAMod = NULL;

    if (myTSanSyncClockRecorder)
        destroySubModuleInstance((I_Module*)myTSanSyncClockRecorder);
    myTSanSyncClockRecorder = NULL;

    if (myVCMod)
        destroySubModuleInstance((I_Module*)myVCMod);
    myVCMod = NULL;

    if (skiplist)
        delete skiplist;
    skiplist = NULL;
}
