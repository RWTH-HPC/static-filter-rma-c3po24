/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: ompt
// RUN: env OMP_NUM_THREADS=4 %must-run --must:mpimode MPMD --must:openmp %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/Locks_nested_no_error 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR)}}' --implicit-check-not 'BAD TERMINATION' %s

#include <mpi.h>
#include <omp.h>

auto main(int argc, char** argv) -> int
{
    int threadlevel_provided = MPI_THREAD_SINGLE - 1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &threadlevel_provided);
    int rank = -1;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    auto lock = omp_nest_lock_t{};
    omp_init_nest_lock(&lock);

#pragma omp parallel
    {
        constexpr unsigned ROUNDS = 100U;
        constexpr unsigned NESTING_DEPTH = 10;
        for (auto i = 0U; i < ROUNDS; ++i) {
            for (auto depth = 0U; depth < NESTING_DEPTH; ++depth)
                omp_set_nest_lock(&lock);
            for (auto depth = 0U; depth < NESTING_DEPTH; ++depth)
                omp_unset_nest_lock(&lock);
        }
    }

    omp_destroy_nest_lock(&lock);
    MPI_Finalize();
}
