/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_TransferTracer.h
 *       @see I_TransferTracer.
 *
 *  @date 16.09.2011
 *  @author Tobias Hilbrich, Mathias Korepkat, Joachim Protze
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"
#include "I_ChannelId.h"

#ifndef I_TRANSFERTRACER_H
#define I_TRANSFERTRACER_H

/**
 * Tracer for communication interval statistic events.
 *
 * Dependencies (order as listed):
 * ---- NONE ----
 */
class I_TransferTracer : public gti::I_Module
{
  public:
    /**
     * Notification of a new interval.
     *
     * @param sentP2P total number of bytes sent with P2P ops.
     * @param numP2P total number of P2P ops (sends) in this interval.
     * @param sentColl total number of bytes sent with collectives (minimal number) in this
     * interval.
     * @param numColl total number of collectives in this interval.
     * @param duration of this interval measured on rank 0.
     * @param thisChannel channel id of this event.
     */
    virtual gti::GTI_ANALYSIS_RETURN interval(
        uint64_t sentP2P,
        uint64_t numP2P,
        uint64_t sentColl,
        uint64_t numColl,
        uint64_t duration,
        gti::I_ChannelId* thisChannel) = 0;

}; /*class I_TransferTracer*/

#endif /*I_TRANSFERTRACER_H*/
