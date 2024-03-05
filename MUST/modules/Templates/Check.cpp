/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file Template.cpp
 *       @see MUST::Template.
 *
 *  @date 01.03.2011
 *  @author Mathias Korepkat, Tobias Hilbrich
 */

#include "GtiMacros.h"
#include "Template.h"
#include "MustEnums.h"
#include "PrefixedOstream.hpp"

#include <sstream>

using namespace must;

mGET_INSTANCE_FUNCTION(Template);
mFREE_INSTANCE_FUNCTION(Template);
mPNMPI_REGISTRATIONPOINT_FUNCTION(Template);

//=============================
// Constructor
//=============================
Template::Template(const char* instanceName) : gti::ModuleBase<Template, I_Template>(instanceName)
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
    if (subModInstances.size() < /*!NUMBER-OF-SUB-MODULES!*/) {
        must::cerr << "Module has not enough sub modules, check its analysis specification! ("
                   << __FILE__ << "@" << __LINE__ << ")" << std::endl;
        assert(0);
    }
    if (subModInstances.size() > /*!NUMBER-OF-SUB-MODULES!*/) {
        for (int i = /*!NUMBER-OF-SUB-MODULES!*/; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myPIdMod = (I_ParallelIdAnalysis*)subModInstances[0];
    myLogger = (I_CreateMessage*)subModInstances[1];
    myArgMod = (I_ArgumentAnalysis*)subModInstances[2];
    /*TODO further modules*/

    // Initialize module data
    /*TODO initialize module data*/
}

//=============================
// Destructor
//=============================
Template::~Template()
{
    if (myPIdMod)
        destroySubModuleInstance((I_Module*)myPIdMod);
    myPIdMod = NULL;

    if (myLogger)
        destroySubModuleInstance((I_Module*)myLogger);
    myLogger = NULL;

    if (myArgMod)
        destroySubModuleInstance((I_Module*)myArgMod);
    myArgMod = NULL;

    /*TODO Free other modules and other data*/
}

//=============================
// analysisFunction
//=============================
GTI_ANALYSIS_RETURN Template::analysisFunction(MustParallelId pId, MustLocationId lId, int aId)
{
    /*TODO Implement and rename*/

    return GTI_ANALYSIS_SUCCESS;
}

/*EOF*/
