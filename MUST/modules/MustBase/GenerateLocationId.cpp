/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file GenerateLocationId.cpp
 *       @see must::GenerateLocationId.
 *
 *  @date 09.02.2023
 *  @author Simon Schwitanski
 */

#include "GtiMacros.h"
#include "MustDefines.h"
#include "PrefixedOstream.hpp"
#include <assert.h>
#include <atomic>
#include <pnmpi.h>

#include "GenerateLocationId.h"

using namespace must;

mGET_INSTANCE_FUNCTION(GenerateLocationId)
mFREE_INSTANCE_FUNCTION(GenerateLocationId)
mPNMPI_REGISTRATIONPOINT_FUNCTION(GenerateLocationId)

//=============================
// Constructor
//=============================
GenerateLocationId::GenerateLocationId(const char* instanceName)
    : gti::ModuleBase<GenerateLocationId, I_GenerateLocationId, false>(instanceName),
      nextLocationId(0)
{
}

//=============================
// getNextLocationId
//==========================
MustLocationId GenerateLocationId::getNextLocationId() { return nextLocationId++; }

/*EOF*/
