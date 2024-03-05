/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef I_MUST_OpenMP_sanity_H
#define I_MUST_OpenMP_sanity_H

#include "BaseIds.h"
#include "GtiEnums.h"
#include "ModuleBase.h"
#include <I_Module.h>

namespace must
{
/**
 * Interface for the OpenMP sanity analysis module.
 *
 * The OpenMP sanity analysis checks, whether MPI and OpenMP are used correctly
 * in multi-threaded applications.
 */
class I_OpenMPsanity : public gti::I_Module
{
  public:
    /**
     * Notify the module about a threaded MPI initialization.
     *
     * If the MPI runtime is initialized for use in multi-threaded applications,
     * this method gets notified to store the threading level provided by the
     * runtime.
     *
     *
     * @param providedLevel Thread support level provided by the MPI runtime.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN notifyThreadedMPI(int providedLevel) = 0;

    /**
     * Notify the module about the begin of a parallel region.
     *
     * If the MPI runtime is initialized for a single-threaded application,
     * there must be no parallel regions in it. Therefore, this method is
     * triggered for each parallel region, checking the compliance with the
     * thread support level provided by the MPI runtime.
     *
     *
     * @param pId Rank and thread of the call.
     * @param lId Source location of the call.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    notifyParallelBegin(MustParallelId pId, MustLocationId lId) = 0;

    /**
     * Check the MPI thread support level before executing the MPI call.
     *
     * This method checks, whether the MPI call is allowed to be executed in the
     * current multi-threaded configuration according to the MPI thread support
     * level provided by the MPI runtime.
     *
     *
     * @param pId Rank and thread of the MPI call.
     * @param lId Source location of the MPI call.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN enterMPICall(MustParallelId pId, MustLocationId lId) = 0;

    /**
     * Check the MPI thread support level after executing the MPI call.
     *
     * This method checks, whether the MPI call is allowed to be executed in the
     * current multi-threaded configuration according to the MPI thread support
     * level provided by the MPI runtime.
     *
     * @param pId Rank and thread of the MPI call.
     * @param lId Source location of the MPI call.
     *
     * @return The status of this analysis.
     */
    virtual gti::GTI_ANALYSIS_RETURN leaveMPICall(MustParallelId pId, MustLocationId lId) = 0;
};
} // namespace must

#endif
