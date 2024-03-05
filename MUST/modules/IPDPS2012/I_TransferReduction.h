/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_TransferReduction.h
 *       @see I_TransferReduction.
 *
 *  @date 16.09.2011
 *  @author Tobias Hilbrich, Mathias Korepkat, Joachim Protze
 */

#include "I_Module.h"
#include "I_Reduction.h"
#include "I_ChannelId.h"
#include "GtiEnums.h"

#include <list>

#ifndef I_TRANSFERREDUCTION_H
#define I_TRANSFERREDUCTION_H

/**
 * Reduction for communication statistics intervals.
 *
 * Dependencies (order as listed):
 * ---- NONE -----
 */
class I_TransferReduction : public gti::I_Module, public gti::I_Reduction
{
  public:
    /**
     * Reduction function.
     *
     * @param sentP2P total number of bytes sent with P2P ops.
     * @param numP2P total number of P2P ops (sends) in this interval.
     * @param sentColl total number of bytes sent with collectives (minimal number) in this
     * interval.
     * @param numColl total number of collectives in this interval.
     * @param duration of this interval measured on rank 0.
     * @param thisChannel @see gti::I_Reduction.
     * @param outFinishedChannels @see gti::I_Reduction.
     */
    virtual gti::GTI_ANALYSIS_RETURN reduce(
        uint64_t sentP2P,
        uint64_t numP2P,
        uint64_t sentColl,
        uint64_t numColl,
        uint64_t duration,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels) = 0;

}; /*class I_TransferReduction*/

#endif /*I_TRANSFERREDUCTION_H*/
