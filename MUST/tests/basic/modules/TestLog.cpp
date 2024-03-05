/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TestLog.cpp
 *       @see MUST::TestLog.
 *
 *  @date 20.01.2010
 *  @author Tobias Hilbrich
 */

#include "GtiMacros.h"
#include "ModuleBase.h"
#include "I_CreateMessage.h"

#include "I_TestLog.h"
#include "MustEnums.h"

using namespace gti;

namespace must
{
/**
     * Prints a location and parallel id to std::cout.
     */
class TestLog : public gti::ModuleBase<TestLog, I_TestLog>
{
  public:
    /**
         * Constructor.
         * @param instanceName name of this module instance.
         */
    TestLog(const char* instanceName);

    /**
    		 * Destructor.
    		 */
    ~TestLog();

    /**
    		 * @see I_TestLog::test
    		 */
    GTI_ANALYSIS_RETURN test(MustParallelId pId, MustLocationId lId);

  protected:
    I_CreateMessage* myLogCreator;
}; /*class TestLog */
} // namespace must

using namespace must;

mGET_INSTANCE_FUNCTION(TestLog);
mFREE_INSTANCE_FUNCTION(TestLog);
mPNMPI_REGISTRATIONPOINT_FUNCTION(TestLog);

//=============================
// Constructor
//=============================
TestLog::TestLog(const char* instanceName) : gti::ModuleBase<TestLog, I_TestLog>(instanceName)
{
    //create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    assert(subModInstances.size() >= 1);

    myLogCreator = (I_CreateMessage*)subModInstances[0];
}

//=============================
// Destructor
//=============================
TestLog::~TestLog() { destroySubModuleInstance((I_Module*)myLogCreator); }

//=============================
// print
//=============================
GTI_ANALYSIS_RETURN TestLog::test(MustParallelId pId, MustLocationId lId)
{

    std::list<std::pair<MustParallelId, MustLocationId>> refs;
    refs.push_back(std::make_pair(pId, lId));
    refs.push_back(std::make_pair(pId, lId));
    myLogCreator->createMessage(
        MUST_MESSAGE_NO_ERROR,
        pId,
        lId,
        MustInformationMessage,
        "Test log message!",
        refs);

    return GTI_ANALYSIS_SUCCESS;
}

/*EOF*/
