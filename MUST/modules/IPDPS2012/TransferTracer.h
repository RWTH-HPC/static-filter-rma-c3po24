/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TransferTracer.h
 *       @see MUST::TransferTracer.
 *
 *  @date 16.09.2011
 *  @author Tobias Hilbrich, Joachim Protze, Mathias Korepkat
 */

#include "ModuleBase.h"
#include "CompletionTree.h"

#include "I_TransferTracer.h"

#include <fstream>

#ifndef TRANSFERTRACER_H
#define TRANSFERTRACER_H

using namespace gti;

namespace must
{
/**
 * Helper struct for ongoing intervals.
 */
class IntervalInfo
{
  public:
    IntervalInfo(
        I_ChannelId* id,
        uint64_t sentP2P,
        uint64_t numP2P,
        uint64_t sentColl,
        uint64_t numColl,
        uint64_t duration);
    ~IntervalInfo(void);

    CompletionTree* completion;
    uint64_t sentP2P;
    uint64_t numP2P;
    uint64_t sentColl;
    uint64_t numColl;
    uint64_t duration;
};

/**
 * Implementation of I_TransferTracer.
 */
class TransferTracer : public gti::ModuleBase<TransferTracer, I_TransferTracer>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    TransferTracer(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~TransferTracer(void);

    /**
     * @see I_TransferTracer::interval
     */
    GTI_ANALYSIS_RETURN interval(
        uint64_t sentP2P,
        uint64_t numP2P,
        uint64_t sentColl,
        uint64_t numColl,
        uint64_t duration,
        gti::I_ChannelId* thisChannel);

  protected:
    std::list<IntervalInfo*> myActiveCompletions;
    std::ofstream myTrace;

    void logNewInterval(
        uint64_t sentP2P,
        uint64_t numP2P,
        uint64_t sentColl,
        uint64_t numColl,
        uint64_t duration);
};
} // namespace must

#endif /*TEMPLATE_H*/
