/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MUST_OpenMP_sanity_H
#define MUST_OpenMP_sanity_H

#include "I_CreateMessage.h"
#include "I_OpenMPsanity.h"
#include "ModuleBase.h"
#include <GtiEnums.h>
#include <memory>

namespace must
{

namespace detail
{
class I_SanityCheck
{
  public:
    virtual auto notify_init_thread() -> gti::GTI_ANALYSIS_RETURN = 0;
    virtual auto notify_parallel_begin(MustParallelId pId, MustLocationId lId)
        -> gti::GTI_ANALYSIS_RETURN = 0;
    virtual auto enter_mpi_call(MustParallelId pId, MustLocationId lId)
        -> gti::GTI_ANALYSIS_RETURN = 0;
    virtual auto leave_mpi_call(MustParallelId pId, MustLocationId lId)
        -> gti::GTI_ANALYSIS_RETURN = 0;

    virtual ~I_SanityCheck() = default;
};
} // namespace detail

/**
 * OpenMP sanity analysis module.
 *
 * The MPI standard defines multiple levels of thread support. This module
 * checks, whether MPI and OpenMP are used correctly in multi-threaded
 * applications.
 *
 *
 * @note This module is intended to be used for hybrid applications, using MPI
 *       and OpenMP. Applications using OpenMP only are NOT supported by this
 *       analysis.
 */
class OpenMPsanity final : public gti::ModuleBase<OpenMPsanity, I_OpenMPsanity>
{
  public:
    /**
     * Constructor.
     *
     * @param instanceName name of this module instance.
     */
    explicit OpenMPsanity(const char* instanceName);

    OpenMPsanity(const OpenMPsanity&) = delete;
    auto operator=(const OpenMPsanity&) -> OpenMPsanity& = delete;

    OpenMPsanity(OpenMPsanity&&) = delete;
    auto operator=(OpenMPsanity&&) -> OpenMPsanity& = delete;

    ~OpenMPsanity() override;

    /**
     * @copydoc I_OpenMPsanity::notifyThreadedMPI
     *
     *
     * @return This function always returns @ref GTI_RETURN_SUCCESS.
     */
    gti::GTI_ANALYSIS_RETURN notifyThreadedMPI(int providedLevel) override;

    /**
     * @copydoc I_OpenMPsanity::notifyParallelBegin
     */
    gti::GTI_ANALYSIS_RETURN notifyParallelBegin(MustParallelId pId, MustLocationId lId) override;

    /**
     * @copydoc I_OpenMPsanity::enterMPICall
     */
    gti::GTI_ANALYSIS_RETURN enterMPICall(MustParallelId pId, MustLocationId lId) override;

    /**
     * @copydoc I_OpenMPsanity::leaveMPICall
     */
    gti::GTI_ANALYSIS_RETURN leaveMPICall(MustParallelId pId, MustLocationId lId) override;

  protected:
    /**
     * Message logger.
     */
    I_CreateMessage* logger;

  private:
    std::unique_ptr<detail::I_SanityCheck> myThreadLevelCheck;
};
} // namespace must

#endif
