/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TargetChecks.cpp
 *       @see must::TargetChecks.
 *
 *  @date 13.06.2017
 *  @author Simon Schwitanski
 */

#include "GtiMacros.h"
#include "TargetChecks.h"
#include "MustEnums.h"
#include "MustDefines.h"
#include "pnmpi/service.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <sstream>
#include <fstream>

using namespace must;

mGET_INSTANCE_FUNCTION(TargetChecks)
mFREE_INSTANCE_FUNCTION(TargetChecks)
mPNMPI_REGISTRATIONPOINT_FUNCTION(TargetChecks)

//=============================
// Constructor.
//=============================
TargetChecks::TargetChecks(const char* instanceName)
    : ModuleBase<TargetChecks, I_TargetChecks>(instanceName)
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

    PNMPI_modHandle_t handle_dummy;
    if (PNMPI_Service_GetModuleByName("libTSanMessages", &handle_dummy) == PNMPI_SUCCESS)
        myHasTSanMessages = true;
    else
        myHasTSanMessages = false;

    myPIdMod = (I_ParallelIdAnalysis*)subModInstances[0];
    myLogger = (I_CreateMessage*)subModInstances[1];
    myConsts = (I_BaseConstants*)subModInstances[2];
    myDatMod = (I_DatatypeTrack*)subModInstances[3];
    myReqMod = (I_RequestTrack*)subModInstances[4];
    myLIdMod = (I_LocationAnalysis*)subModInstances[5];
    myWinMod = (I_WinTrack*)subModInstances[6];
    myTSanMod = (I_TSan*)subModInstances[7];
    myRMAMod = (I_RMATrack*)subModInstances[8];
    myTSanSyncClockRecorder = (I_TSanSyncClockRecorder*)subModInstances[9];
    myVCMod = (I_VectorClock*)subModInstances[10];
}

GTI_ANALYSIS_RETURN TargetChecks::winCreate(MustWinType win, void* ann)
{

    myWinLockAddrs[win] = ann;
    myWinCreateAddrs[win] = ann;

    return GTI_ANALYSIS_SUCCESS;
}

GTI_ANALYSIS_RETURN TargetChecks::winLock(
    MustParallelId pId,
    MustLocationId lId,
    int lock_type,
    int rank,
    MustWinType win,
    void* ann)
{
    int realRank = translateRank(myWinMod->getWin(pId, win)->getComm(), rank);

    if (lock_type == MPI_LOCK_EXCLUSIVE) {
        // if this is a local win lock, store TSan annotation address
        if (realRank == myPIdMod->getInfoForId(pId).rank) {
            std::cout << "insert (win " << win << "): " << ann << std::endl;
            myWinLockAddrs[win] = ann;
        }
    }

    return GTI_ANALYSIS_SUCCESS;
}

/**
 * @see I_TargetChecks::targetOpStart
 */
GTI_ANALYSIS_RETURN TargetChecks::targetOpStart(MustRMAId rmaId)
{
    I_TargetRMAOpPersistent* op = myRMAMod->getPersistentTargetRMAOp(rmaId);

    // resolve clock value to TSan sync clock
    if (op->getOrigin() == op->getTarget()) {
        // Special case 1: Local call, the call itself is the earliest point in terms of
        // synchronization, so get its sync clock.
        op->setFiber(myRMAMod->getOriginRMAOp(rmaId)->getFiber());
    } else if (op->getClock() == 0) {
        // Special case 2: If origin clock is 0, both processes never synchronized, so we take the
        // window creation function as start of the concurrent region.
        op->setFiber(myWinCreateAddrs[op->getWinId()]);
    } else {
        // Usual case: Get TSan sync clock from clock value
        op->setFiber(myTSanSyncClockRecorder->getTSanSyncClock(op->getClock()));
        // std::cout << ", start vector clock: " <<
        // myTSanSyncClockRecorder->getVCSyncClock(op->getClock()).toStr();
    }

#ifdef MUST_DEBUG
    std::stringstream msg;
    msg << "TargetRMAOp STARTED: ";
    msg << "callId: " << rmaId;
    msg << ", pId: " << op->getPId();
    msg << ", lId: " << op->getLId();
    msg << ", isStore: " << op->isStore();
    msg << ", win: " << op->getWin();
    msg << ", target: " << op->getTarget();
    msg << ", annotation address: " << op->getFiber();
    msg << std::endl;
    std::cout << msg.str();
#endif

    return GTI_ANALYSIS_SUCCESS;
}

/**
 * @see I_TargetChecks::targetOpComplete
 */
GTI_ANALYSIS_RETURN TargetChecks::targetOpComplete(
    MustParallelId pId,
    MustLocationId lId,
    MustRMAId* rmaId,
    int rmaIdLen)
{
#ifdef MUST_DEBUG
    std::cout << "=== TARGET OPS COMPLETED ===" << std::endl;
#endif
    // get current fiber to switch back at the end
    myTSanMod->annotateHappensBefore(pId, lId, rmaId);
    void* curFiber = myTSanMod->getCurrentFiber();

    for (int i = 0; i < rmaIdLen; ++i) {
        I_TargetRMAOp* op = myRMAMod->getTargetRMAOp(rmaId[i]);

#ifdef MUST_DEBUG
        std::stringstream msg;
        msg << "TargetRMAOp COMPLETED: ";
        msg << "callId: " << rmaId[i];
        msg << ", pId: " << op->getPId();
        msg << ", lId: " << op->getLId();
        msg << ", isStore: " << op->isStore();
        msg << ", isLocked: " << op->isLocked();
        msg << ", win: " << op->getWin();
        msg << ", win id: " << op->getWinId();
        msg << ", target: " << op->getTarget();
        msg << ", annotation address: " << op->getFiber();
        msg << ", return addr: " << op->getReturnAddr();
        msg << ", function addr: " << op->getFunctionAddr();
        msg << ", start vector clock: "
            << myTSanSyncClockRecorder->getVCSyncClock(op->getClock()).toStr();
        msg << ", RMA vector clock (from origin): " << op->getVectorClock().toStr();
        msg << ", end vector clock: " << myVCMod->getClock().toStr() << std::endl;
        std::cout << msg.str();
#endif
        // select fiber
        void* tempFiber;

        if (myFiberPool.find(op->getOrigin()) != myFiberPool.end()) {
            // fiber already created for origin rank
            tempFiber = myFiberPool[op->getOrigin()];
        } else {
            // create new fiber for origin rank
            myTSanMod->annotateIgnoreSyncBegin();
            tempFiber = myTSanMod->createFiber(0);

            char procName[32];
            snprintf(procName, 32, "rank %d", op->getOrigin());
            myTSanMod->setFiberName(tempFiber, procName);

            myFiberPool.insert(std::make_pair(op->getOrigin(), tempFiber));
            myTSanMod->annotateIgnoreSyncEnd();
        }

        // switch to fiber of operation without synchronization (flag: 1)
        myTSanMod->switchToFiber(tempFiber, 1);

        // load corresponding vector clock
        myTSanMod->annotateHappensAfter(pId, lId, op->getFiber());

        // acquire lock if operation is locked and there is a local lock annotation
        const bool acquireLock = op->isLocked() && myWinLockAddrs[op->getWinId()];
        if (acquireLock) {
            const void* lock = myWinLockAddrs[op->getWinId()];
            myTSanMod->annotateRWLockAcquired(op->getPId(), op->getLId(), lock, true);
        }

        // ignore any synchronization implied while annotating mem access (otherwise we might run
        // into false negatives due to usage of safeptr etc.)
        myTSanMod->annotateIgnoreSyncBegin();

        // annotate atomic accesses separately
        if (op->isAtomic()) {
            MustMpiDatatypePredefined baseType = extractBasetype(op->getTargetDatatype());
            annotateAtomicAccesses(
                op->getPId(),
                op->getLId(),
                op->isStore(),
                op->getMemIntervals(),
                baseType);
        } else {
            annotateMemAccess(
                op->getPId(),
                op->getLId(),
                op->getMemIntervals(),
                op->isStore(),
                op->getReturnAddr(),
                op->getFunctionAddr());
        }

        myTSanMod->annotateIgnoreSyncEnd();

        // release acquired lock
        if (acquireLock) {
            const void* lock = myWinLockAddrs[op->getWinId()];
            myTSanMod->annotateRWLockReleased(op->getPId(), op->getLId(), lock, true);
        }
    }

    // switch to current thread *with* synchronization
    // (memory accesses afterwards do *not* conflict with *this* RMA call)
    myTSanMod->annotateHappensBefore(pId, lId, rmaId);
    myTSanMod->switchToFiber(curFiber, 0);
    myTSanMod->annotateHappensAfter(pId, lId, rmaId);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// annotateAtomicAccesses
//=============================
void TargetChecks::annotateAtomicAccesses(
    MustParallelId pId,
    MustLocationId lId,
    bool isStore,
    const MustMemIntervalListType& memIntervals,
    MustMpiDatatypePredefined baseType)
{
    this->annotateFuncEntry(pId, lId);

#ifdef MUST_DEBUG
    for (MustMemIntervalListType::iterator it = memIntervals.begin(); it != memIntervals.end();
         ++it) {
        std::stringstream msg;
        msg << "stride: " << it->stride;
        msg << ", blocksize: " << it->blocksize;
        msg << ", count: " << it->count;
        msg << ", repetition: " << it->repetition;
        msg << std::endl;
        std::cout << msg.str();
    }
#endif

    switch (baseType) {
    // TODO: check for problems in shadow memory when annotating bytewise accesses
    case MUST_MPI_CHAR: // 8 bit
    case MUST_MPI_SIGNED_CHAR:
    case MUST_MPI_UNSIGNED_CHAR:
    case MUST_MPI_BYTE:
    case MUST_MPI_PACKED:
    case MUST_MPI_INT8_T:
    case MUST_MPI_UINT8_T:
    case MUST_MPI_C_BOOL:
        for (MustMemIntervalListType::iterator it = memIntervals.begin(); it != memIntervals.end();
             ++it) {
            for (int i = 0; i < it->count; ++i) {
                MustAddressType start = it->baseAddress + (it->stride * i);
                MustAddressType end = start + it->blocksize;
                for (MustAddressType curAddr = start; curAddr < end; curAddr += 1) {
                    if (isStore)
                        myTSanMod->annotateAtomic8Store((unsigned char*)curAddr);
                    else
                        myTSanMod->annotateAtomic8Load((unsigned char*)curAddr);
                }
            }
        }

        break;

    case MUST_MPI_SHORT: // 16 bit
    case MUST_MPI_UNSIGNED_SHORT:
    case MUST_MPI_WCHAR:
    case MUST_MPI_INT16_T:
    case MUST_MPI_UINT16_T:
        for (MustMemIntervalListType::iterator it = memIntervals.begin(); it != memIntervals.end();
             ++it) {
            for (int i = 0; i < it->count; ++i) {
                MustAddressType start = it->baseAddress + (it->stride * i);
                MustAddressType end = start + it->blocksize;
                for (MustAddressType curAddr = start; curAddr < end; curAddr += 2) {
                    if (isStore)
                        myTSanMod->annotateAtomic16Store((unsigned short*)curAddr);
                    else
                        myTSanMod->annotateAtomic16Load((unsigned short*)curAddr);
                }
            }
        }

        break;

    case MUST_MPI_INT: // 32 bit
    case MUST_MPI_UNSIGNED:
    case MUST_MPI_LONG:
    case MUST_MPI_UNSIGNED_LONG:
    case MUST_MPI_FLOAT:
    case MUST_MPI_INT32_T:
    case MUST_MPI_UINT32_T:
        for (MustMemIntervalListType::iterator it = memIntervals.begin(); it != memIntervals.end();
             ++it) {
            for (int i = 0; i < it->count; ++i) {
                MustAddressType start = it->baseAddress + (it->stride * i);
                MustAddressType end = start + it->blocksize;
                for (MustAddressType curAddr = start; curAddr < end; curAddr += 4) {
                    if (isStore)
                        myTSanMod->annotateAtomic32Store((unsigned int*)curAddr);
                    else
                        myTSanMod->annotateAtomic32Load((unsigned int*)curAddr);
                }
            }
        }

        break;

    case MUST_MPI_LONG_LONG: // 64 bit
    case MUST_MPI_LONG_LONG_INT:
    case MUST_MPI_LONG_DOUBLE:
    case MUST_MPI_UNSIGNED_LONG_LONG:
    case MUST_MPI_DOUBLE:
    case MUST_MPI_INT64_T:
    case MUST_MPI_UINT64_T:
        for (MustMemIntervalListType::iterator it = memIntervals.begin(); it != memIntervals.end();
             ++it) {
            for (int i = 0; i < it->count; ++i) {
                MustAddressType start = it->baseAddress + (it->stride * i);
                MustAddressType end = start + it->blocksize;
                for (MustAddressType curAddr = start; curAddr < end; curAddr += 8) {
                    if (isStore)
                        myTSanMod->annotateAtomic64Store((unsigned long long*)curAddr);
                    else
                        myTSanMod->annotateAtomic64Load((unsigned long long*)curAddr);
                }
            }
        }

        break;

    default:
        std::cout << "Error TargetChecks: Unsupported base type (id = " << baseType
                  << ") for atomic annotation.";
        break;
    }
    this->annotateFuncExit();
}

//=============================
// annotateMemAccess
//=============================
void TargetChecks::annotateMemAccess(
    MustParallelId pId,
    MustLocationId lId,
    const MustMemIntervalListType& memIntervals,
    bool isStore,
    const void* returnAddr,
    const void* funcAddr)
{
    this->annotateFuncEntry(pId, lId);
    for (MustMemIntervalListType::iterator it = memIntervals.begin(); it != memIntervals.end();
         ++it) {
#ifdef MUST_DEBUG
        std::stringstream msg;
        msg << "stride: " << it->stride;
        msg << ", blocksize: " << it->blocksize;
        msg << ", count: " << it->count;
        msg << ", repetition: " << it->repetition;
        msg << std::endl;
        std::cout << msg.str();
#endif

        for (int i = 0; i < it->count; ++i) {
            MustAddressType start = it->baseAddress + (it->stride * i);
#ifdef MUST_DEBUG
            std::stringstream msg;
            msg << "[" << start << "," << start + it->blocksize << "]" << std::endl;
            msg << "[" << (void*)start << "," << (void*)(start + it->blocksize) << "]" << std::endl;
            std::cout << msg.str();
#endif
            // Increment function address by one here, because TSan expects a return address and
            // decrements the pointer by one.
            if (isStore)
                myTSanMod->annotateMemoryWritePC(
                    pId,
                    lId,
                    start,
                    it->blocksize,
                    (char*)(myTSanMod->translateCallPtr(funcAddr)) + 1);
            else
                myTSanMod->annotateMemoryReadPC(
                    pId,
                    lId,
                    start,
                    it->blocksize,
                    (char*)(myTSanMod->translateCallPtr(funcAddr)) + 1);
        }
    }
    this->annotateFuncExit();
}

//=============================
// extractBasetype
//=============================
MustMpiDatatypePredefined TargetChecks::extractBasetype(I_Datatype* datatype)
{
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
// annotateFuncEntry
//=============================
void TargetChecks::annotateFuncEntry(MustParallelId pId, MustLocationId lId)
{
    myTSanMod->annotateFuncEntry(pId, lId);
    if (myHasTSanMessages) {
        // Encode pId and lId in TSan stacktrace reports by marking them with a special delimiter
        // 0x0FFFFFFFFFFFFFFF such that we can detect that a race with an annotated call has been
        // detected. This allows us to extract the pId and lId later on when interpreting the TSan
        // report.
        myTSanMod->annotateFuncEntry((void*)lId);
        myTSanMod->annotateFuncEntry((void*)pId);
        myTSanMod->annotateFuncEntry((void*)0x0FFFFFFFFFFFFFFF);
    }
}

//=============================
// annotateFuncExit
//=============================
void TargetChecks::annotateFuncExit()
{
    myTSanMod->annotateFuncExit();
    if (myHasTSanMessages) {
        myTSanMod->annotateFuncExit();
        myTSanMod->annotateFuncExit();
        myTSanMod->annotateFuncExit();
    }
}

//=============================
// translateRank
//=============================
int TargetChecks::translateRank(I_Comm* comm, int rank)
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
TargetChecks::~TargetChecks(void)
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

    if (myTSanMod)
        destroySubModuleInstance((I_Module*)myTSanMod);
    myTSanMod = NULL;

    if (myRMAMod)
        destroySubModuleInstance((I_Module*)myRMAMod);
    myRMAMod = NULL;

    if (myTSanSyncClockRecorder)
        destroySubModuleInstance((I_Module*)myTSanSyncClockRecorder);
    myTSanSyncClockRecorder = NULL;
}
