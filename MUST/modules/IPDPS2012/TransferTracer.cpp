/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TransferTracer.cpp
 *       @see MUST::TransferTracer.
 *
 *  @date 16.09.2011
 *  @author Tobias Hilbrich, Mathias Korepkat, Joachim Protze
 */

#include "GtiMacros.h"
#include "MustEnums.h"

#include "TransferTracer.h"

using namespace must;

mGET_INSTANCE_FUNCTION(TransferTracer)
mFREE_INSTANCE_FUNCTION(TransferTracer)
mPNMPI_REGISTRATIONPOINT_FUNCTION(TransferTracer)

//=============================
// Constructor -- IntervalInfo
//=============================
IntervalInfo::IntervalInfo(
    I_ChannelId* id,
    uint64_t sentP2P,
    uint64_t numP2P,
    uint64_t sentColl,
    uint64_t numColl,
    uint64_t duration)
    : completion(NULL), sentP2P(sentP2P), numP2P(numP2P), sentColl(sentColl), numColl(numColl),
      duration(duration)
{
    if (id)
        completion = new CompletionTree(
            id->getNumUsedSubIds() - 1,
            id->getSubIdNumChannels(id->getNumUsedSubIds() - 1));
}

//=============================
// Destructor -- IntervalInfo
//=============================
IntervalInfo::~IntervalInfo(void)
{
    if (completion)
        delete (completion);
    completion = NULL;
}

//=============================
// Constructor
//=============================
TransferTracer::TransferTracer(const char* instanceName)
    : ModuleBase<TransferTracer, I_TransferTracer>(instanceName), myTrace()
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
    //  no sub modules

    /*No sub modules to store*/

    // Initialize module data
    myTrace.open("IPDPS_Profiler.trace");
}

//=============================
// Destructor
//=============================
TransferTracer::~TransferTracer()
{
    /*No sub modules to free*/

    // Free other module data
    myTrace.close();
    myActiveCompletions.clear();
}

//=============================
// interval
//=============================
GTI_ANALYSIS_RETURN TransferTracer::interval(
    uint64_t sentP2P,
    uint64_t numP2P,
    uint64_t sentColl,
    uint64_t numColl,
    uint64_t duration,
    gti::I_ChannelId* thisChannel)
{
    // If this is not a completely reduced interval, then we have to gather a bit more
    if (thisChannel) {
        // Does this belongs to any ongoing reduction ?
        std::list<IntervalInfo*>::iterator iter;

        for (iter = myActiveCompletions.begin(); iter != myActiveCompletions.end(); iter++) {
            IntervalInfo* i = *iter;
            CompletionTree* t = (*iter)->completion;

            if (t->wasCompleted(thisChannel))
                continue;

            t->addCompletion(thisChannel);

            if (duration != 0)
                i->duration = duration;
            i->sentP2P += sentP2P;
            i->numP2P += numP2P;
            i->sentColl += sentColl;
            i->numColl += numColl;

            if (t->isCompleted()) {
                logNewInterval(i->sentP2P, i->numP2P, i->sentColl, i->numColl, i->duration);
                delete (i);
                myActiveCompletions.erase(iter);
            }

            return GTI_ANALYSIS_SUCCESS;
        }

        // Add as a new ongoing interval reduction
        myActiveCompletions.push_back(
            new IntervalInfo(thisChannel, sentP2P, numP2P, sentColl, numColl, duration));
        myActiveCompletions.back()->completion->addCompletion(thisChannel);

        return GTI_ANALYSIS_SUCCESS;
    }

    // This is a complete wave
    logNewInterval(sentP2P, numP2P, sentColl, numColl, duration);
    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// logNewInterval
//=============================
void TransferTracer::logNewInterval(
    uint64_t sentP2P,
    uint64_t numP2P,
    uint64_t sentColl,
    uint64_t numColl,
    uint64_t duration)
{
    myTrace << duration << " " << sentP2P << " " << numP2P << " " << sentColl << " " << numColl
            << " " << std::endl;
}

/*EOF*/
