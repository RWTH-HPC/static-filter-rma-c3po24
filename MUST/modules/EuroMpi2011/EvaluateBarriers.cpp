/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file EvaluateBarriers.cpp
 *       @see MUST::EvaluateBarriers.
 *
 *  @date 06.05.2011
 *  @author Tobias Hilbrich
 */

#include "GtiMacros.h"
#include "PrefixedOstream.hpp"

#include "EvaluateBarriers.h"

#include <sstream>

using namespace must;

mGET_INSTANCE_FUNCTION(EvaluateBarriers)
mFREE_INSTANCE_FUNCTION(EvaluateBarriers)
mPNMPI_REGISTRATIONPOINT_FUNCTION(EvaluateBarriers)

//=============================
// Constructor
//=============================
EvaluateBarriers::EvaluateBarriers(const char* instanceName)
    : gti::ModuleBase<EvaluateBarriers, I_EvaluateBarriers>(instanceName), mySumDilation(0),
      myNumBarriers(0), myMinDilation(-1), myMaxDilation(-1)
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUB_MODS 0
    if (subModInstances.size() > NUM_SUB_MODS) {
        for (std::vector<I_Module*>::size_type i = NUM_SUB_MODS; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }
}

//=============================
// Destructor
//=============================
EvaluateBarriers::~EvaluateBarriers()
{
    must::cout << "=================" << std::endl
               << "        Barrier evaluation" << std::endl
               << "=================" << std::endl
               << "Num Barriers: " << myNumBarriers << std::endl
               << "Avg Dilation: " << (double)mySumDilation / (double)myNumBarriers << std::endl
               << "Min Dilation: " << myMinDilation << std::endl
               << "Max Dilation: " << myMaxDilation << std::endl
               << "=================" << std::endl;
}

//=============================
// newBarrier
//=============================
GTI_ANALYSIS_RETURN EvaluateBarriers::newBarrier(unsigned long long tMin, unsigned long long tMax)
{
    long long d = tMax - tMin;

    if (d < myMinDilation || myMinDilation < 0)
        myMinDilation = d;

    if (d > myMaxDilation || myMaxDilation < 0)
        myMaxDilation = d;

    mySumDilation += d;
    myNumBarriers++;

    return GTI_ANALYSIS_SUCCESS;
}

/*EOF*/
