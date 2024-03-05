/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file BarrierReduction.h
 *       @see MUST::BarrierReduction.
 *
 *  @date 06.05.2011
 *  @author Tobias Hilbrich
 */

#include "ModuleBase.h"
#include "CompletionTree.h"

#include "I_BarrierReduction.h"

#ifndef BARRIERREDUCTION_H
#define BARRIERREDUCTION_H

using namespace gti;

namespace must
{
/**
 * Implementation of I_CommReduction.
 */
class BarrierReduction : public gti::ModuleBase<BarrierReduction, I_BarrierReduction>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    BarrierReduction(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~BarrierReduction(void);

    /**
     * @see I_BarrierReduction::reduce
     */
    GTI_ANALYSIS_RETURN reduce(
        unsigned long long tMin,
        unsigned long long tMax,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels);

    /**
     * The timeout function, see gti::I_Reduction.timeout
     */
    void timeout(void);

  protected:
    bool myIsFirst;
    unsigned long long myTMin;
    unsigned long long myTMax;
    std::list<I_ChannelId*> myReductionPartners;

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
};
} // namespace must

#endif /*BARRIERREDUCTION_H*/
