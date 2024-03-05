/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TestLocation.cpp
 *       @see MUST::TestLocation.
 *
 *  @date 10.01.2010
 *  @author Tobias Hilbrich
 */

#include "GtiMacros.h"
#include "ModuleBase.h"
#include "I_LocationAnalysis.h"
#include "I_ParallelIdAnalysis.h"

#include "I_TestLocation.h"

using namespace gti;

namespace must
{
/**
     * Prints a location and parallel id to std::cout.
     */
class TestLocation : public gti::ModuleBase<TestLocation, I_TestLocation>
{
  public:
    /**
         * Constructor.
         * @param instanceName name of this module instance.
         */
    TestLocation(const char* instanceName);

    /**
    		 * Destructor.
    		 */
    ~TestLocation();

    /**
    		 * @see I_TestLocation::print
    		 */
    GTI_ANALYSIS_RETURN print(MustParallelId pId, MustLocationId lId);

  protected:
    I_ParallelIdAnalysis* myPIdModule;
    I_LocationAnalysis* myLIdModule;
}; /*class TestLocation */
} // namespace must

using namespace must;

mGET_INSTANCE_FUNCTION(TestLocation);
mFREE_INSTANCE_FUNCTION(TestLocation);
mPNMPI_REGISTRATIONPOINT_FUNCTION(TestLocation);

//=============================
// Constructor
//=============================
TestLocation::TestLocation(const char* instanceName)
    : gti::ModuleBase<TestLocation, I_TestLocation>(instanceName)
{
    //create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    assert(subModInstances.size() >= 2);

    myPIdModule = (I_ParallelIdAnalysis*)subModInstances[0];
    myLIdModule = (I_LocationAnalysis*)subModInstances[1];
}

//=============================
// Destructor
//=============================
TestLocation::~TestLocation()
{
    destroySubModuleInstance((I_Module*)myPIdModule);
    destroySubModuleInstance((I_Module*)myLIdModule);
}

//=============================
// print
//=============================
GTI_ANALYSIS_RETURN TestLocation::print(MustParallelId pId, MustLocationId lId)
{
    std::cout << "Location: " << myLIdModule->toString(pId, lId) << " from "
              << myPIdModule->toString(pId) << std::endl;

    return GTI_ANALYSIS_SUCCESS;
}

/*EOF*/
