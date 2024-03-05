/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file BarrierReduction.cpp
 *       @see MUST::BarrierReduction.
 *
 *  @date 06.05.2011
 *  @author Tobias Hilbrich
 */

#include "GtiMacros.h"
#include "MustEnums.h"
#include "BarrierReductionApi.h"

#include "BarrierReduction.h"

using namespace must;

mGET_INSTANCE_FUNCTION(BarrierReduction)
mFREE_INSTANCE_FUNCTION(BarrierReduction)
mPNMPI_REGISTRATIONPOINT_FUNCTION(BarrierReduction)

//=============================
// Constructor
//=============================
BarrierReduction::BarrierReduction(const char* instanceName)
    : gti::ModuleBase<BarrierReduction, I_BarrierReduction>(instanceName), myIsFirst(true),
      myTMin(0), myTMax(0), myReductionPartners(), myCompletion(NULL), myTimedOutReds()
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
    if (subModInstances.size() > 0) {
        for (std::vector<I_Module*>::size_type i = 0; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    // Initialize module data
    // nothing to do
}

//=============================
// Destructor
//=============================
BarrierReduction::~BarrierReduction()
{
    // Clear completion tree
    if (myCompletion)
        delete myCompletion;
    myCompletion = NULL;

    // Clear all outstanding reduction partners, there should be none in common situations
    std::list<I_ChannelId*>::iterator iter;
    for (iter = myReductionPartners.begin(); iter != myReductionPartners.end(); iter++) {
        if (*iter)
            delete (*iter);
    }
    myReductionPartners.clear();

    // Clear all timed out reductions
    std::list<CompletionTree*>::iterator iter2;
    for (iter2 = myTimedOutReds.begin(); iter2 != myTimedOutReds.end(); iter2++) {
        if (*iter2)
            delete *iter2;
    }
    myTimedOutReds.clear();
}

//=============================
// reduce
//=============================
GTI_ANALYSIS_RETURN BarrierReduction::reduce(
    unsigned long long tMin,
    unsigned long long tMax,
    gti::I_ChannelId* thisChannel,
    std::list<gti::I_ChannelId*>* outFinishedChannels)
{
    // 1)====================================
    // Is this a record that was missing in a timed out reduction ?
    std::list<CompletionTree*>::iterator timedIter;
    for (timedIter = myTimedOutReds.begin(); timedIter != myTimedOutReds.end(); timedIter++) {
        CompletionTree* current = *timedIter;

        // Was it already completed in the tree ?
        if (current->wasCompleted(thisChannel))
            continue;

        // Was not completed in that tree, add completion
        current->addCompletion(thisChannel);

        // Is the reduction fininshed now ?
        if (current->isCompleted()) {
            delete current;
            myTimedOutReds.erase(timedIter);
        }

        return GTI_ANALYSIS_IRREDUCIBLE;
    }

    // 2)====================================
    // This is a record of an active and possibly successful reduction
    CompletionTree* tree = getCompletionTree(thisChannel);
    tree->addCompletion(thisChannel);

    if (myIsFirst) {
        myTMin = tMin;
        myTMax = tMax;
        myIsFirst = false;
    } else {
        if (tMin < myTMin)
            myTMin = tMin;

        if (tMax > myTMax)
            myTMax = tMax;
    }

    if (tree->isCompleted()) {
        myIsFirst = true;

        // Reset completions tree
        tree->flushCompletions();

        // Sum up and add finished partners to out list
        std::list<I_ChannelId*>::iterator i;
        for (i = myReductionPartners.begin(); i != myReductionPartners.end(); i++) {
            // set as reopened channel
            outFinishedChannels->push_back(*i);
        }

        // Important: clear stored partners ...
        myReductionPartners.clear();

        // Call creation of reduced record, get the wrapp everywhere function from the wrapper for
        // that
        reducedBarrierP fp;
        if (getWrapperFunction("reducedBarrier", (GTI_Fct_t*)&fp) == GTI_SUCCESS)
            (*fp)(myTMin, myTMax);

        return GTI_ANALYSIS_SUCCESS;
    }

    // Waiting for more, add to list of blocked partners
    myReductionPartners.push_back(thisChannel);
    return GTI_ANALYSIS_WAITING;
}

//=============================
// timeout
//=============================
void BarrierReduction::timeout(void)
{
    // Did we start a reduction at all (if not, nothing to do here)
    if (myReductionPartners.size() > 0) {
        myIsFirst = true;

        // remove old partners
        std::list<I_ChannelId*>::iterator i;
        for (i = myReductionPartners.begin(); i != myReductionPartners.end(); i++)
            delete (*i);
        myReductionPartners.clear();

        // move old completion tree to list of timed out reductions
        if (myCompletion)
            myTimedOutReds.push_back(myCompletion);
        myCompletion = NULL;
    }
}

//=============================
// getCompletionTree
//=============================
CompletionTree* BarrierReduction::getCompletionTree(I_ChannelId* id)
{
    if (!myCompletion)
        myCompletion = new CompletionTree(
            id->getNumUsedSubIds() - 1,
            id->getSubIdNumChannels(id->getNumUsedSubIds() - 1));
    return myCompletion;
}

/*EOF*/
