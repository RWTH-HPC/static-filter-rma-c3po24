/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file CollectUnmapped.cpp
 *       @see MUST::CollectUnmapped.
 *
 *  @date 07.07.2022
 *  @author Felix Tomski
 */

#include "GtiMacros.h"
#include "CollectUnmapped.h"
#include "MustEnums.h"
#include "PrefixedOstream.hpp"

#include <sstream>

using namespace must;

mGET_INSTANCE_FUNCTION(CollectUnmapped)
mFREE_INSTANCE_FUNCTION(CollectUnmapped)
mPNMPI_REGISTRATIONPOINT_FUNCTION(CollectUnmapped)

//=============================
// Constructor
//=============================
CollectUnmapped::CollectUnmapped(const char* instanceName)
    : gti::ModuleBase<CollectUnmapped, I_CollectUnmapped>(instanceName), myUnmappedFunctions()
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUBMODULES 3
    if (subModInstances.size() < NUM_SUBMODULES) {
        must::cerr << "Module has not enough sub modules, check its analysis specification! ("
                   << __FILE__ << "@" << __LINE__ << ")" << std::endl;
        assert(0);
    }
    if (subModInstances.size() > NUM_SUBMODULES) {
        for (std::vector<I_Module*>::size_type i = NUM_SUBMODULES; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myPIdMod = (I_ParallelIdAnalysis*)subModInstances[0];
    myLogger = (I_CreateMessage*)subModInstances[1];
    myLIdMod = (I_LocationAnalysis*)subModInstances[2];
}

//=============================
// Destructor
//=============================
CollectUnmapped::~CollectUnmapped()
{
    if (myPIdMod)
        destroySubModuleInstance((I_Module*)myPIdMod);
    myPIdMod = NULL;

    if (myLogger)
        destroySubModuleInstance((I_Module*)myLogger);
    myLogger = NULL;

    if (myLIdMod)
        destroySubModuleInstance((I_Module*)myLIdMod);
    myLIdMod = NULL;
}

//=============================
// collectFunction
//=============================
GTI_ANALYSIS_RETURN CollectUnmapped::collectFunction(MustParallelId pId, MustLocationId lId)
{
    myUnmappedFunctions.emplace(
        myLIdMod->getInfoForId(pId, lId).callName,
        std::make_pair(pId, lId));
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// printUnmappedFunctions
//=============================
GTI_ANALYSIS_RETURN CollectUnmapped::printUnmappedFunctions(MustParallelId pId, MustLocationId lId)
{
    /* Do not print a message if there were not unmapped functions detected. */
    if (!myUnmappedFunctions.size())
        return GTI_ANALYSIS_SUCCESS;

    std::stringstream stream;
    stream << "The following MPI functions were used but are not supported by MUST: " << std::endl;

    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    for (const auto& f : myUnmappedFunctions) {
        stream << f.first << std::endl;
        refs.emplace_back(f.second);
    }

    myLogger->createMessage(
        MUST_INFO_UNIMPLEMENTED_FEATURE,
        pId,
        lId,
        MustInformationMessage,
        stream.str(),
        refs);

    return GTI_ANALYSIS_SUCCESS;
}

/*EOF*/
