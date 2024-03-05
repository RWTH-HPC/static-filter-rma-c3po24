/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TSan.cpp
 *       @see MUST::TSan.
 *
 *  @date 18.05.2017
 *  @author Simon Schwitanski
 */

#include "GtiMacros.h"
#include "TSan.h"
#include "MustEnums.h"

#include <sstream>
#include <string.h>

using namespace must;

mGET_INSTANCE_FUNCTION(TSan)
mFREE_INSTANCE_FUNCTION(TSan)
mPNMPI_REGISTRATIONPOINT_FUNCTION(TSan)

//=============================
// Constructor
//=============================

TSan::TSan(const char* instanceName) : gti::ModuleBase<TSan, I_TSan>(instanceName)
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUB_MODULES 3
    if (subModInstances.size() < NUM_SUB_MODULES) {
        std::cerr << "Module has not enough sub modules, check its analysis specification! ("
                  << __FILE__ << "@" << __LINE__ << ")" << std::endl;
        assert(0);
    }
    if (subModInstances.size() > NUM_SUB_MODULES) {
        for (std::vector<I_Module*>::size_type i = NUM_SUB_MODULES; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myLogger = (I_CreateMessage*)subModInstances[0];
    myLIdMod = (I_LocationAnalysis*)subModInstances[1];
    myPIdMod = (I_ParallelIdAnalysis*)subModInstances[2];

    // initFuncNameModule();
    // Initialize module data
    /*Nothing to do*/
}

//=============================
// Destructor
//=============================
TSan::~TSan()
{
    if (myLogger)
        destroySubModuleInstance((I_Module*)myLogger);
    myLogger = NULL;

    if (myLIdMod)
        destroySubModuleInstance((I_LocationAnalysis*)myLIdMod);
    myLIdMod = NULL;

    if (myPIdMod)
        destroySubModuleInstance((I_ParallelIdAnalysis*)myPIdMod);
    myPIdMod = NULL;
}

void TSan::annotateHappensBefore(MustParallelId pId, MustLocationId lId, const volatile void* cv)
{
    TsanHappensBefore(cv);
}

void TSan::annotateHappensAfter(MustParallelId pId, MustLocationId lId, const volatile void* cv)
{
    TsanHappensAfter(cv);
}

void TSan::annotateInitTLC(MustParallelId pId, MustLocationId lId, const volatile void* cv)
{
    TsanInitTLC(cv);
}

void TSan::annotateStartTLC(MustParallelId pId, MustLocationId lId, const volatile void* cv)
{
    TsanStartTLC(cv);
}

void TSan::annotateMemoryRead(
    MustParallelId pId,
    MustLocationId lId,
    MustAddressType addr,
    size_t size)
{
    TsanMemoryRead((void*)addr, (uptr)size);
}

void TSan::annotateMemoryReadPC(
    MustParallelId pId,
    MustLocationId lId,
    MustAddressType addr,
    size_t size,
    void* pc)
{
    TsanMemoryReadPC((void*)addr, (uptr)size, pc);
}

void TSan::annotateMemoryWrite(
    MustParallelId pId,
    MustLocationId lId,
    MustAddressType addr,
    size_t size)
{
    TsanMemoryWrite((void*)addr, (uptr)size);
}

void TSan::annotateMemoryWritePC(
    MustParallelId pId,
    MustLocationId lId,
    MustAddressType addr,
    size_t size,
    void* pc)
{
    TsanMemoryWritePC((void*)addr, (uptr)size, pc);
}

void TSan::annotateRWLockCreate(MustParallelId pId, MustLocationId lId, const volatile void* cv)
{
    TsanRWLockCreate(cv);
}

void TSan::annotateRWLockDestroy(MustParallelId pId, MustLocationId lId, const volatile void* cv)
{
    TsanRWLockDestroy(cv);
}

void TSan::annotateRWLockAcquired(
    MustParallelId pId,
    MustLocationId lId,
    const volatile void* cv,
    bool isWriteLock)
{
    TsanRWLockAcquired(cv, (unsigned long long)isWriteLock);
}

void TSan::annotateRWLockReleased(
    MustParallelId pId,
    MustLocationId lId,
    const volatile void* cv,
    bool isWriteUnlock)
{
    TsanRWLockReleased(cv, (unsigned long long)isWriteUnlock);
}

void TSan::annotateAtomic8Load(const volatile unsigned char* a) { TsanAtomic8Load(a, mo_relaxed); }

void TSan::annotateAtomic8Store(volatile unsigned char* a) { TsanAtomic8Store(a, *a, mo_release); }

void TSan::annotateAtomic16Load(const volatile unsigned short* a)
{
    TsanAtomic16Load(a, mo_relaxed);
}

void TSan::annotateAtomic16Store(volatile unsigned short* a)
{
    TsanAtomic16Store(a, *a, mo_release);
}

void TSan::annotateAtomic32Load(const volatile unsigned int* a) { TsanAtomic32Load(a, mo_relaxed); }

void TSan::annotateAtomic32Store(volatile unsigned int* a) { TsanAtomic32Store(a, *a, mo_release); }

void TSan::annotateAtomic64Load(const volatile unsigned long long* a)
{
    TsanAtomic64Load(a, mo_relaxed);
}

void TSan::annotateAtomic64Store(volatile unsigned long long* a)
{
    TsanAtomic64Store(a, *a, mo_release);
}

void TSan::annotateFuncEntry(MustParallelId pId, MustLocationId lId)
{
    TsanFuncEntry((void*)translateCodePtr(pId, lId));
}

void TSan::annotateFuncEntry(void* pc) { TsanFuncEntry(pc); }

void TSan::annotateFuncExit() { TsanFuncExit(); }

void TSan::annotateIgnoreReadsBegin() { TsanIgnoreReadsBegin(); }

void TSan::annotateIgnoreReadsEnd() { TsanIgnoreReadsEnd(); }

void TSan::annotateIgnoreWritesBegin() { TsanIgnoreWritesBegin(); }

void TSan::annotateIgnoreWritesEnd() { TsanIgnoreWritesEnd(); }

void TSan::annotateIgnoreSyncBegin() { TsanIgnoreSyncBegin(); }

void TSan::annotateIgnoreSyncEnd() { TsanIgnoreSyncEnd(); }

void* TSan::createFiber(unsigned flags) { return TsanCreateFiber(flags); }

void TSan::destroyFiber(void* fiber) { TsanDestroyFiber(fiber); }

void TSan::switchToFiber(void* fiber, unsigned flags) { TsanSwitchToFiber(fiber, flags); }

void* TSan::getCurrentFiber() { return TsanGetCurrentFiber(); }

void TSan::setFiberName(void* fiber, const char* name) { return TsanSetFiberName(fiber, name); }

void TSan::registerBaseAddress(const char* fName, const void* fBase)
{
    auto x_safe_BaseAddressMap = xlock_safe_ptr(myBaseAddressMap);
    x_safe_BaseAddressMap->insert(std::make_pair(fName, (intptr_t)fBase));
}

const void* TSan::translateCallPtr(const void* callptr)
{
    void* baseptr;
    PNMPI_Service_GetSelfBaseAddress(&baseptr);

    return (void*)((intptr_t)(baseptr) + (intptr_t)(callptr));
}

const void* TSan::translateCodePtr(MustParallelId pId, MustLocationId lId)
{
    LocationInfo info = myLIdMod->getInfoForId(pId, lId);

    // Immediately return if we know we are on the same rank, then no translation is necessary
    MustParallelId curPid;
    getNodeInLayerId(&curPid);
    if (pId == curPid || myPIdMod->getInfoForId(pId).rank == curPid) {
        return info.codeptr;
    }

    // Otherwise translate codeptr
    intptr_t oldbase = (intptr_t)info.fbase;
    intptr_t oldcodeptr = (intptr_t)info.codeptr;

    auto s_safe_BaseAddressMap = slock_safe_ptr(myBaseAddressMap);
    auto it = s_safe_BaseAddressMap->find(info.fname);
    if (it == s_safe_BaseAddressMap->end())
        assert(0);

    intptr_t newbase = it->second;

    if (oldcodeptr > oldbase) {
        // Clang 13: shift oldcodeptr first and then add newbase
        return (const void*)(oldcodeptr - oldbase + newbase);
    } else {
        // Clang 15: no shifting required, since oldcodeptr is
        // already relative to binary start
        return (const void*)(oldcodeptr + newbase);
    }
}

/*EOF*/
