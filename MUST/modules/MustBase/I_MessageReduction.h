/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_MessageReduction.h
 *       @see I_MessageReduction
 *
 *  @date 02.08.2013
 *  @author Tobias Hilbrich
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "I_Reduction.h"
#include "BaseIds.h"

#include "I_CreateMessage.h"

#include <list>

#ifndef I_MESSAGEREDUCTION_H
#define I_MESSAGEREDUCTION_H

/**
 * Interface that tries to aggregate similar messages.
 *
 * Dependencies:
 * - LocationAnalysis
 * - ParallelIdAnalysis
 * - FinishNotify
 */
class I_MessageReduction : public gti::I_Module, public gti::I_Reduction
{
  public:
    typedef std::list<std::pair<MustParallelId, MustLocationId>> RefListType;

    /**
     * @see I_MessageLogger::log
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN reduce(
        int msgId,
        int hasLocation,
        uint64_t pId,
        uint64_t lId,
        size_t fileId,
        int msgType,
        char* text,
        int textLen,
        int numReferences,
        uint64_t* refPIds,
        uint64_t* refLIds,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels) = 0;

    /**
     * Version of reduce for aggregated messages
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN reduceStrided(
        int msgId,
        uint64_t pIdRef,
        uint64_t lIdRef,
        size_t fileId,
        int startRank,
        int stride,
        int count,
        int msgType,
        char* text,
        int textLen,
        int numReferences,
        uint64_t* refPIds,
        uint64_t* refLIds,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels) = 0;

}; /*class I_MessageReduction*/

#endif /*I_MESSAGEREDUCTION_H*/
