/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TransferReduction.h
 *       @see MUST::TransferReduction.
 *
 *  @date 16.09.2011
 *  @author Tobias Hilbrich, Joachim Protze, Mathias Korepkat
 */

#include "ModuleBase.h"
#include "CompletionTree.h"
#include "TransferReductionApi.h"

#include "I_TransferReduction.h"

#ifndef TRANSFERREDUCTION_H
#define TRANSFERREDUCTION_H

using namespace gti;

namespace must
{
/**
 * Template for analysis interface implementation.
 */
class TransferReduction : public gti::ModuleBase<TransferReduction, I_TransferReduction>
{
  protected:
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    TransferReduction(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~TransferReduction(void);

    /**
     * @see I_TransferReduction::reduce
     */
    GTI_ANALYSIS_RETURN reduce(
        uint64_t sentP2P,
        uint64_t numP2P,
        uint64_t sentColl,
        uint64_t numColl,
        uint64_t duration,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels);

    /**
     * The timeout function, see gti::I_Reduction.timeout
     */
    void timeout(void);

  protected:
    uint64_t mySentP2P;
    uint64_t myNumP2P;
    uint64_t mySentColl;
    uint64_t myNumColl;
    uint64_t myDuration;
    std::list<I_ChannelId*> myReductionPartners;

    statisticsIntervalP myFP; /**< Function pointer to create an interval statistics.*/

    /**
     * Completion tree, IMPORTANT: we only do one single reduction here,
     * it either fails or succeeds, so we don't need a history of timed-out
     * reductions here.
     */
    CompletionTree* myCompletion;
    std::list<CompletionTree*> myTimedOutReds;

    /**
     * Returns the current or a new completion tree.
     * @param any id, used to determine size of root node.
     * @return completion tree.
     */
    CompletionTree* getCompletionTree(I_ChannelId* id);

}; /*class TransferReduction */
} // namespace must

#endif /*TRANSFERREDUCTION_H*/
