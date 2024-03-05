/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: ompt
// RUN: env OMP_NUM_THREADS=4 %must-run --must:mpimode MPMD --must:openmp %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/Locks_foreign_unlock 2>&1 \
// RUN: | %filecheck-may-segfault %s

#include <mpi.h>
#include <omp.h>
#include <iostream>

auto main(int argc, char** argv) -> int
{
    int threadlevel_provided = MPI_THREAD_SINGLE - 1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &threadlevel_provided);
    int rank = -1;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (omp_get_max_threads() < 2) {
        std::cerr << "This test requires at least two threads!" << std::endl;
        MPI_Finalize();
        return -1;
    }

    auto lock = omp_lock_t{};
    omp_init_lock(&lock);

#pragma omp parallel
    {
        if (omp_get_thread_num() == 0) {
            omp_set_lock(&lock);
        }
#pragma omp barrier
        if (omp_get_thread_num() == 1) {
            // CHECK: Error: from: {{.*}}: Lock was locked by other thread than the one who is unlocking.
            omp_unset_lock(&lock);
        }
    }

    omp_destroy_lock(&lock);
    MPI_Finalize();
}
