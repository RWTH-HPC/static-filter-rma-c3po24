/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TSanSyncClockRecorder.cpp
 *       @see must::TSanSyncClockRecorder.
 *
 *  @date 12.11.2021
 *  @author Simon Schwitanski
 */

#include "GtiMacros.h"
#include "TSanSyncClockRecorder.h"
#include "MustEnums.h"
#include "MustDefines.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "PrefixedOstream.hpp"

#include <cassert>
#include <iostream>

using namespace must;

mGET_INSTANCE_FUNCTION(TSanSyncClockRecorder)
mFREE_INSTANCE_FUNCTION(TSanSyncClockRecorder)
mPNMPI_REGISTRATIONPOINT_FUNCTION(TSanSyncClockRecorder)

//=============================
// Constructor
//=============================
TSanSyncClockRecorder::TSanSyncClockRecorder(const char* instanceName)
    : ModuleBase<TSanSyncClockRecorder, I_TSanSyncClockRecorder>(instanceName)
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUBMODULES 1
    if (subModInstances.size() < NUM_SUBMODULES) {
        must::cerr << "Module has not enough sub modules, check its analysis specification! ("
                   << __FILE__ << "@" << __LINE__ << ")" << std::endl;
        assert(0);
    }
    if (subModInstances.size() > NUM_SUBMODULES) {
        for (std::vector<I_Module*>::size_type i = NUM_SUBMODULES; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myVCMod = (I_VectorClock*)subModInstances[0];
}

//=============================
// Destructor
//=============================
TSanSyncClockRecorder::~TSanSyncClockRecorder(void)
{
    if (myVCMod)
        destroySubModuleInstance((I_Module*)myVCMod);
    myVCMod = NULL;
}

//=============================
// getTSanSyncClock
//=============================
void* TSanSyncClockRecorder::getTSanSyncClock(int clock) const
{
#ifdef MUST_DEBUG
    std::cout << "[SyncClockRecorder] Look up for clock value " << clock << std::endl;
#endif
    auto it = syncTSanClocks.find(clock);

    if (it == syncTSanClocks.end()) {
        must::cerr << "Could not find TSan sync clock for vector clock value " << clock
                   << std::endl;
        assert(0);
    }

    return it->second;
}

//=============================
// getVCSyncClock
//=============================
const Clock& TSanSyncClockRecorder::getVCSyncClock(int clock) const
{
#ifdef MUST_DEBUG
    std::cout << "[SyncClockRecorder] Look up for clock value " << clock << std::endl;
#endif
    auto it = syncVCClocks.find(clock);

    if (it == syncVCClocks.end()) {
        must::cerr << "Could not find VC sync clock for vector clock value " << clock << std::endl;
        assert(0);
    }

    return it->second;
}

//=============================
// storeSyncClock
//=============================
GTI_ANALYSIS_RETURN TSanSyncClockRecorder::storeSyncClock(void* ann)
{
    // Store TSan sync clock address for the current VC value
    syncTSanClocks[myVCMod->getLocalClockValue()] = ann;
    syncVCClocks[myVCMod->getLocalClockValue()] = myVCMod->getClock();

#ifdef MUST_DEBUG
    std::cout << "[SyncClockRecorder] Store ann=" << ann << " with clock value "
              << myVCMod->getLocalClockValue() << std::endl;
#endif

    return GTI_ANALYSIS_SUCCESS;
}
