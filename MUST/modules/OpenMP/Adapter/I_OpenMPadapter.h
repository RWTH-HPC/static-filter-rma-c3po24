/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef I_MUST_OpenMP_adapter_H
#define I_MUST_OpenMP_adapter_H

#include <I_Module.h>
#include <ModuleBase.h>

namespace must
{
/**
 * Interface for the OpenMP tools interface adapter.
 *
 * The OpenMP tools interface adapter needs to be loaded by both MUST and the
 * OpenMP runtime. Therefore, a minimal interface is required, to be loaded by
 * the weaver.
 */
class I_OpenMPadapter : public gti::I_Module
{
  public:
    /**
     * Notify the analysis about the MPI stack being shutdown.
     *
     * This method get's triggert in the MPI_Finalize event and should inform
     * the analysis to shutdown the OpenMP environment.
     */
    virtual void finish() = 0;
};
} // namespace must

#endif
