/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MUST_OpenMP_adapter_H
#define MUST_OpenMP_adapter_H

#include "BaseIds.h"
#include "MustTypes.h"
#include "I_InitLocationId.h"
#include "I_InitParallelId.h"
#include "I_OpenMPadapter.h"
#include "ModuleBase.h"
#include "omp-tools.h"
#include <cstdint>

namespace must
{
/**
 * OpenMP tools interface adapter.
 *
 * This class provides the MUST internal logic for relaying OMPT callbacks into
 * the MUST stack.
 */
class OpenMPadapter final : public gti::ModuleBase<OpenMPadapter, I_OpenMPadapter>
{
  public:
    /**
     * Constructor.
     *
     *
     * @param instanceName name of this module instance
     */
    OpenMPadapter(const char* instanceName);

    ~OpenMPadapter() override;

    /**
     * @copydoc I_OpenMPIadapter::finish
     */
    void finish() override;

    /**
     * Get the parallel ID for a specific call.
     *
     *
     * @return The parallel ID for the thread executing the call.
     */
    MustParallelId getParallelId();

    /**
     * Get the location ID for a specific call.
     *
     *
     * @param codeptr_ra The source code location pointer.
     *
     * @return The location ID indicating the origin of the call.
     */
    MustLocationId getLocationId(const void* codeptr_ra);

    /**
     * MUST version of `ompt_callback_mutex_acquire_t`.
     *
     * This typedef will be used for all `ompt_callback_mutex_acquire_t` typed
     * OMPT callbacks. These will be split into several callbacks depending on
     * the `kind` parameter of the original callback.
     */
    using callback_mutex_acquire_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        unsigned int hint,
        unsigned int impl,
        ompt_wait_id_t wait_id,
        MustAddressType codeptr_ra);

    /* Split callbacks of ompt_callback_mutex_acquire. */
    callback_mutex_acquire_t callback_wait_lock = nullptr;
    callback_mutex_acquire_t callback_wait_nest_lock = nullptr;
    callback_mutex_acquire_t callback_wait_critical = nullptr;
    callback_mutex_acquire_t callback_wait_atomic = nullptr;
    callback_mutex_acquire_t callback_wait_ordered = nullptr;

    /* Split callbacks of ompt_callback_lock_init. */
    callback_mutex_acquire_t callback_init_lock = nullptr;
    callback_mutex_acquire_t callback_init_nest_lock = nullptr;

    /**
     * MUST version of `ompt_callback_mutex_t`.
     *
     * This typedef will be used for all `ompt_callback_mutex_t` typed OMPT
     * callbacks. These will be split into several callbacks depending on the
     * `kind` parameter of the original callback.
     */
    using callback_mutex_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        ompt_wait_id_t wait_id,
        MustAddressType codeptr_ra);

    /* Split callbacks of ompt_callback_mutex_acquired. */
    callback_mutex_t callback_acquired_lock = nullptr;
    callback_mutex_t callback_acquired_nest_lock = nullptr;
    callback_mutex_t callback_acquired_critical = nullptr;
    callback_mutex_t callback_acquired_atomic = nullptr;
    callback_mutex_t callback_acquired_ordered = nullptr;

    /* Split callbacks of ompt_callback_mutex_released. */
    callback_mutex_t callback_release_lock = nullptr;
    callback_mutex_t callback_release_nest_lock = nullptr;
    callback_mutex_t callback_release_critical = nullptr;
    callback_mutex_t callback_release_atomic = nullptr;
    callback_mutex_t callback_release_ordered = nullptr;

    /* Split callbacks of ompt_callback_nest_lock. */
    callback_mutex_t callback_acquired_nest_lock_next = nullptr;
    callback_mutex_t callback_release_nest_lock_prev = nullptr;

    /* Split callbacks of ompt_callback_lock_destroy. */
    callback_mutex_t callback_destroy_lock = nullptr;
    callback_mutex_t callback_destroy_nest_lock = nullptr;

    /**
     * MUST version of `ompt_callback_sync_region_t`.
     *
     * This typedef will be used for all `ompt_callback_sync_region_t` typed
     * OMPT callbacks. These will be split into several callbacks depending on
     * the `endpoint` and `kind` parameters of the original callback.
     */
    using callback_sync_region_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        ompt_sync_region_t kind,
        uint64_t parallel_data,
        uint64_t task_data,
        MustAddressType codeptr_ra);

    /* Split callbacks of ompt_callback_sync_region. */
    callback_sync_region_t callback_barrier_begin = nullptr;
    callback_sync_region_t callback_barrier_end = nullptr;

    callback_sync_region_t callback_taskwait_begin = nullptr;
    callback_sync_region_t callback_taskwait_end = nullptr;

    callback_sync_region_t callback_taskgroup_begin = nullptr;
    callback_sync_region_t callback_taskgroup_end = nullptr;

    /* Split callbacks of ompt_callback_sync_region_wait. */
    callback_sync_region_t callback_wait_barrier_begin = nullptr;
    callback_sync_region_t callback_wait_barrier_end = nullptr;

    callback_sync_region_t callback_wait_taskwait_begin = nullptr;
    callback_sync_region_t callback_wait_taskwait_end = nullptr;

    callback_sync_region_t callback_wait_taskgroup_begin = nullptr;
    callback_sync_region_t callback_wait_taskgroup_end = nullptr;

    /**
     * MUST version of `ompt_callback_flush_t`.
     */
    using callback_flush_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t thread_data,
        MustAddressType codeptr_ra);

    callback_flush_t callback_flush = nullptr;

    /**
     * MUST version of `ompt_callback_cancel_t`.
     */
    using callback_cancel_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t task_data,
        int flags,
        MustAddressType codeptr_ra);

    callback_cancel_t callback_cancel = nullptr;

    /**
     * MUST version of `ompt_callback_implicit_task_t`.
     *
     * This typedef will be used for all `ompt_callback_implicit_task_t` typed
     * OMPT callbacks. These will be split into several callbacks depending on
     * the `endpoint` and `flags` parameters of the original callback.
     */
    using callback_implicit_task_t = void (*)(
        MustParallelId pId,
        uint64_t parallel_data,
        uint64_t task_data,
        unsigned int team_size,
        unsigned int thread_num,
        int flags);

    /* Split callbacks of ompt_callback_implicit_task. */
    callback_implicit_task_t callback_initial_task_begin = nullptr;
    callback_implicit_task_t callback_initial_task_end = nullptr;
    callback_implicit_task_t callback_implicit_task_begin = nullptr;
    callback_implicit_task_t callback_implicit_task_end = nullptr;

    /**
     * MUST version of `ompt_callback_work_t`.
     *
     * This typedef will be used for all `ompt_callback_work_t` typed OMPT
     * callbacks. These will be split into several callbacks depending on the
     * `endpoint` and `wstype` parameters of the original callback.
     */
    using callback_work_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t parallel_data,
        uint64_t task_data,
        uint64_t count,
        MustAddressType codeptr_ra);

    /* Split callbacks of ompt_callback_work. */
    callback_work_t callback_loop_begin = nullptr;
    callback_work_t callback_loop_end = nullptr;

    callback_work_t callback_sections_begin = nullptr;
    callback_work_t callback_sections_end = nullptr;

    callback_work_t callback_single_in_block_begin = nullptr;
    callback_work_t callback_single_in_block_end = nullptr;

    callback_work_t callback_single_others_begin = nullptr;
    callback_work_t callback_single_others_end = nullptr;

    callback_work_t callback_distribute_begin = nullptr;
    callback_work_t callback_distribute_end = nullptr;

    callback_work_t callback_taskloop_begin = nullptr;
    callback_work_t callback_taskloop_end = nullptr;

    /**
     * MUST version of `ompt_callback_masked_t`.
     *
     * This typedef will be used for all `ompt_callback_masked_t` typed OMPT
     * callbacks. These will be split into several callbacks depending on the
     * `endpoint` parameter of the original callback.
     */
    using callback_masked_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t parallel_data,
        uint64_t task_data,
        MustAddressType codeptr_ra);

    /* Split callbacks of ompt_callback_masked. */
    callback_masked_t callback_masked_begin = nullptr;
    callback_masked_t callback_masked_end = nullptr;

    /**
     * MUST version of `ompt_callback_parallel_begin_t`.
     */
    using callback_parallel_begin_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t encountering_task_data,
        uint64_t parallel_data,
        uint32_t requested_team_size,
        int flag,
        MustAddressType codeptr_ra);

    callback_parallel_begin_t callback_parallel_begin = nullptr;

    /**
     * MUST version of `ompt_callback_parallel_end_t`.
     */
    using callback_parallel_end_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t parallel_data,
        uint64_t encountering_task_data,
        int flag,
        MustAddressType codeptr_ra);

    callback_parallel_end_t callback_parallel_end = nullptr;

    /**
     * MUST version of `ompt_callback_task_create_t`.
     */
    using callback_task_create_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t encountering_task_data,
        uint64_t new_task_data,
        int type,
        int has_dependences,
        MustAddressType codeptr_ra);

    callback_task_create_t callback_task_create = nullptr;

    /**
     * MUST version of `ompt_callback_task_schedule_t`.
     */
    using callback_task_schedule_t = void (*)(
        MustParallelId pId,
        uint64_t first_task_data,
        ompt_task_status_t prior_task_status,
        uint64_t second_task_data);

    callback_task_schedule_t callback_task_schedule = nullptr;

    /**
     * MUST version of `ompt_callback_task_end_t`.
     */
    using callback_task_end_t = void (*)(MustParallelId pId, uint64_t first_task_data);

    callback_task_end_t callback_task_end = nullptr;

    /**
     * MUST version of `ompt_callback_dependences_t`.
     */
    using callback_dependences_t =
        void (*)(MustParallelId pId, uint64_t task_data, const ompt_dependence_t* deps, int ndeps);

    callback_dependences_t callback_task_dependences = nullptr;

    /**
     * MUST version of `ompt_callback_task_dependence_t`.
     */
    using callback_task_dependence_t =
        void (*)(MustParallelId pId, uint64_t first_task_data, uint64_t second_task_data);

    callback_task_dependence_t callback_task_dependence_pair = nullptr;

    /**
     * MUST version of `ompt_callback_thread_begin_t`.
     */
    using callback_thread_begin_t =
        void (*)(MustParallelId pId, ompt_thread_t thread_type, uint64_t thread_data);

    callback_thread_begin_t callback_thread_begin = nullptr;

    /**
     * MUST version of `ompt_callback_thread_end_t`.
     */
    using callback_thread_end_t = void (*)(MustParallelId pId, uint64_t thread_data);

    callback_thread_end_t callback_thread_end = nullptr;

    /**
     * MUST version of `ompt_callback_control_tool_t`.
     */
    using callback_control_tool_t = void (*)(
        MustParallelId pId,
        MustLocationId lId,
        uint64_t command,
        uint64_t modifier,
        void* arg,
        MustAddressType codeptr_ra);

    callback_control_tool_t callback_control_tool = nullptr;

    /**
     * MUST version of `ompt_initialize_t`.
     */
    using callback_initialize_t =
        int (*)(ompt_function_lookup_t lookup, int initial_device_num, ompt_data_t* tool_data);

    callback_initialize_t callback_initialize = nullptr;

    /**
     * MUST version of `ompt_finalize_t`.
     */
    using callback_finalize_t = int (*)(ompt_data_t* tool_data);

    callback_finalize_t callback_finalize = nullptr;

  protected:
    /**
     * I_InitParallelId interface to generated parallel id's when necessary
     */
    I_InitParallelId* parallelInit = nullptr;

    /**
     * I_InitLocationId interface to generated location id's when necessary
     */
    I_InitLocationId* locationInit = nullptr;
};
} // namespace must

#endif
