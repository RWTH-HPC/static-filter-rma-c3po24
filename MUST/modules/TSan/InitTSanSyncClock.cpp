/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file InitTSanSyncClock.cpp
 *       @see InitTSanSyncClock
 *
 *  @date 22.10.22
 *  @author Simon Schwitanski
 */

#include "GtiEnums.h"
#include "GtiMacros.h"
#include "InitTSanSyncClock.h"
#include "MustEnums.h"

#include <sstream>
#include <string.h>

using namespace must;

mGET_INSTANCE_FUNCTION(InitTSanSyncClock)
mFREE_INSTANCE_FUNCTION(InitTSanSyncClock)
mPNMPI_REGISTRATIONPOINT_FUNCTION(InitTSanSyncClock)

//=============================
// Constructor
//=============================
InitTSanSyncClock::InitTSanSyncClock(const char* instanceName)
    : gti::ModuleBase<InitTSanSyncClock, I_InitTSanSyncClock>(instanceName)
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUB_MODULES 1
    if (subModInstances.size() < NUM_SUB_MODULES) {
        std::cerr << "Module has not enough sub modules, check its analysis specification! ("
                  << __FILE__ << "@" << __LINE__ << ")" << std::endl;
        assert(0);
    }
    if (subModInstances.size() > NUM_SUB_MODULES) {
        for (std::vector<I_Module*>::size_type i = NUM_SUB_MODULES; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myTSanMod = (I_TSan*)subModInstances[0];

    // initFuncNameModule();
    // Initialize module data
    /*Nothing to do*/

    // Create data pool for annotation addresses
    myAnnPool = new DataPool<AnnData, 4>();
}

//=============================
// Destructor
//=============================
InitTSanSyncClock::~InitTSanSyncClock()
{
    if (myTSanMod)
        destroySubModuleInstance((I_Module*)myTSanMod);
    myTSanMod = NULL;
}

//=============================
// init
//=============================
GTI_ANALYSIS_RETURN InitTSanSyncClock::init(void** pStorage)
{
    // get address out of DataPool
    void* ann = new AnnData();

    // store current TSan sync clock
    myTSanMod->annotateHappensBefore(0, 0, ann);

    // set storage to address
    *pStorage = ann;

    return GTI_ANALYSIS_SUCCESS;
}