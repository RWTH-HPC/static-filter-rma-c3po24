/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file
 */

#ifndef MUST_OpenMP_barriers_H
#define MUST_OpenMP_barriers_H

#include <map>
#include <mutex>

#include "ModuleBase.h"

#include "I_OpenMPbarriers.h"
#include "I_CreateMessage.h"

namespace must
{
/**
 * OpenMP barrier analysis module.
 *
 * This module tracks the application's use of barriers and raises errors, if
 * threads hit different barriers.
 */
class OpenMPbarriers : public gti::ModuleBase<OpenMPbarriers, I_OpenMPbarriers>
{
  public:
    /**
     * Constructor.
     *
     *
     * @param instanceName name of this module instance.
     */
    OpenMPbarriers(const char* instanceName);

    ~OpenMPbarriers() override;

    /**
     * @copydoc I_OpenMPbarriers::notifyBarrierBegin
     */
    gti::GTI_ANALYSIS_RETURN notifyBarrierBegin(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t parallel_data,
        MustAddressType codeptr_ra) override;

    /**
     * @copydoc I_OpenMPbarriers::notifyBarrierEnd
     */
    gti::GTI_ANALYSIS_RETURN notifyBarrierEnd(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t parallel_data,
        MustAddressType codeptr_ra) override;

  private:
    /**
     * Message logger.
     */
    I_CreateMessage* logger;

    using Region = uint64_t;
    using Barrier = MustAddressType;

    /**
     * Mutex to protect OpenMPbarriers::active_barriers
     */
    static std::mutex active_barriers_mut;
    /**
     * Map of active barriers and the related parallel regions.
     */
    static std::map<Region, Barrier> active_barriers;
};
} // namespace must

#endif
