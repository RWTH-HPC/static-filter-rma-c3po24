/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file PrintCommGroup.cpp
 *       @see MUST::PrintCommGroup.
 *
 *  @date 06.03.2011
 *  @author Tobias Hilbrich
 */

#include "GtiMacros.h"
#include "PrintCommGroup.h"

#include <assert.h>
#include <sstream>

using namespace must;

mGET_INSTANCE_FUNCTION(PrintCommGroup);
mFREE_INSTANCE_FUNCTION(PrintCommGroup);
mPNMPI_REGISTRATIONPOINT_FUNCTION(PrintCommGroup);

//=============================
// Constructor
//=============================
PrintCommGroup::PrintCommGroup(const char* instanceName)
    : gti::ModuleBase<PrintCommGroup, I_PrintCommGroup>(instanceName), myLocations(NULL),
      myLogger(NULL), myCTracker(NULL), myGTracker(NULL), myMsgId(0)
{
    //create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    //handle sub modules
    if (subModInstances.size() < 5) {
        std::cerr << "Error: " << __LINE__ << "@" << __FILE__
                  << " has not enough sub modules, aborting!" << std::endl;
        assert(0);
    }
    if (subModInstances.size() > 5) {
        for (std::vector<I_Module*>::size_type i = 5; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myLocations = (I_LocationAnalysis*)subModInstances[0];
    myLogger = (I_CreateMessage*)subModInstances[1];
    myCTracker = (I_CommTrack*)subModInstances[2];
    myGTracker = (I_GroupTrack*)subModInstances[3];
    myPIdMod = (I_ParallelIdAnalysis*)subModInstances[4];
}

//=============================
// Destructor
//=============================
PrintCommGroup::~PrintCommGroup()
{
    if (myLocations)
        destroySubModuleInstance((I_Module*)myLocations);
    myLocations = NULL;

    if (myLogger)
        destroySubModuleInstance((I_Module*)myLogger);
    myLogger = NULL;

    if (myCTracker)
        destroySubModuleInstance((I_Module*)myCTracker);
    myCTracker = NULL;

    if (myGTracker)
        destroySubModuleInstance((I_Module*)myGTracker);
    myGTracker = NULL;

    if (myPIdMod)
        destroySubModuleInstance((I_Module*)myPIdMod);
    myPIdMod = NULL;
}

//=============================
// printComm
//=============================
GTI_ANALYSIS_RETURN
PrintCommGroup::printComm(MustParallelId pId, MustLocationId lId, MustCommType comm)
{
    std::stringstream stream;
    stream << "Information on communicator: ";
    std::list<std::pair<MustParallelId, MustLocationId>> refs;

    //Extra information for comm contents
    I_Comm* info = myCTracker->getComm(pId, comm);

    if (!info) {
        std::cout << "[" << myPIdMod->getInfoForId(pId).rank << ", " << pId << "]"
                  << "Unknown Communicator: " << comm << std::endl;
    } else {
        info->printInfo(stream, &refs);

        if (info != NULL && !info->isNull()) {
            stream << " (";
            addGroupInfoToStream(info->getGroup(), stream);

            if (info->isIntercomm()) {
                stream << ", remote group: ";
                addGroupInfoToStream(info->getRemoteGroup(), stream);
            }

            stream << " contextId=" << info->getContextId() << ")";
        }
    }

    myLogger->createMessage(getNextMsgId(), MustInformationMessage, stream.str(), refs);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// printGroup
//=============================
GTI_ANALYSIS_RETURN
PrintCommGroup::printGroup(MustParallelId pId, MustLocationId lId, MustGroupType group)
{
    std::stringstream stream;
    stream << "Information on group: ";
    std::list<std::pair<MustParallelId, MustLocationId>> refs;

    I_Group* info = myGTracker->getGroup(pId, group);

    if (!info) {
        stream << "Unknown Group." << std::endl;
    } else {
        info->printInfo(stream, &refs);

        //==1) Invalid
        if (!info->isNull()) {
            //Put information about group layout into stream
            stream << " (";
            addGroupInfoToStream(info->getGroup(), stream);
            stream << ")";
        }
    }

    myLogger->createMessage(getNextMsgId(), MustInformationMessage, stream.str(), refs);

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// addGroupInfoToStream
//=============================
void PrintCommGroup::addGroupInfoToStream(I_GroupTable* group, std::stringstream& stream)
{
    if (group) {
        stream << "size=" << group->getSize() << " table: ";
        for (int i = 0; i < group->getSize(); i++) {
            if (i != 0)
                stream << "; ";

            int t;
            group->translate(i, &t);
            stream << i << "->" << t;
        }
    } else {
        stream << " pointer to I_Group is NULL";
    }
}

MustMessageIdNames PrintCommGroup::getNextMsgId()
{
    int ret = myMsgId;
    myMsgId = (myMsgId + 1) % MUST_LAST_MESSAGE_ID_NAME;
    return static_cast<MustMessageIdNames>(ret);
}

/*EOF*/
