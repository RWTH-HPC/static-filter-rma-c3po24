/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: ompt
// RUN: env OMP_NUM_THREADS=4 %must-run --must:mpimode MPMD --must:openmp %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/Locks_no_error 2>&1 \
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

    auto lock = omp_lock_t{};
    auto lock2 = omp_lock_t{};
    omp_init_lock(&lock);
    omp_init_lock(&lock2);

#pragma omp parallel
    {
        constexpr unsigned ROUNDS = 1000U;
        for (auto i = 0U; i < ROUNDS; ++i) {
            omp_set_lock(&lock);
            omp_set_lock(&lock2);
            omp_unset_lock(&lock2);
            omp_unset_lock(&lock);
        }
        for (auto i = 0U; i < ROUNDS; ++i) {
            omp_set_lock(&lock);
            omp_unset_lock(&lock);
            omp_set_lock(&lock2);
            omp_unset_lock(&lock2);
        }
    }

    omp_destroy_lock(&lock2);
    omp_destroy_lock(&lock);
    MPI_Finalize();
}
