/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_BarrierReduction.h
 *       @see I_BarrierReduction.
 *
 *  @date 06.05.2011
 *  @author Tobias Hilbrich
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"
#include "I_Reduction.h"
#include "I_CommTrack.h"

#include <list>

#ifndef I_BARRIERREDUCTION_H
#define I_BARRIERREDUCTION_H

/**
 * A simple reduction to aggregate barriers.
 *
 * Dependencies (order as listed):
 * X
 */
class I_BarrierReduction : public gti::I_Module, public gti::I_Reduction
{
  public:
    /**
     * Reduction function.
     *
     * @param tMin time at which the earliest process executed the collective.
     * @param tMax time at which the latest process executed the collective.
     * @param thisChannel @see gti::I_Reduction.
     * @param outFinishedChannels @see gti::I_Reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN reduce(
        unsigned long long tMin,
        unsigned long long tMax,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels) = 0;

}; /*class I_BarrierReduction*/

#endif /*I_BARRIERREDUCTION_H*/
