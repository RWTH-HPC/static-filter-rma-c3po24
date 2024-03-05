/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file
 */

#ifndef I_MUST_OpenMP_barriers_H
#define I_MUST_OpenMP_barriers_H

#include <cstdint>

#include "GtiEnums.h"
#include "I_Module.h"

#include "BaseIds.h"
#include "MustTypes.h"

namespace must
{
/**
 * Interface for the OpenMP barrier analysis module.
 *
 * The OpenMP barrier analysis checks, whether barriers are used properly inside
 * the application.
 */
class I_OpenMPbarriers : public gti::I_Module
{
  public:
    /**
     * Notify the module about the begin of a barrier.
     *
     * This method will be called for all threads hitting a specific barrier.
     * All threads of a specific parallel region must hit the same barrier, so
     * for all calls with the same @p parallel_data the @p lId needs to be the
     * same.
     *
     *
     * @param pId Rank and thread of the call.
     * @param lId Source location of the call.
     * @param parallel_data Unique ID of the parallel region of the barrier.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN notifyBarrierBegin(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t parallel_data,
        MustAddressType codeptr_ra) = 0;

    /**
     * Notify the module about the end of a barrier.
     *
     * If all threads of a parallel region hit the same barrier to notify the
     * module about the barrier being successfully passed.
     *
     *
     * @param pId Rank and thread of the call.
     * @param lId Source location of the call.
     * @param parallel_data Unique ID of the parallel region of the barrier.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN notifyBarrierEnd(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t parallel_data,
        MustAddressType codeptr_ra) = 0;
};
} // namespace must

#endif
