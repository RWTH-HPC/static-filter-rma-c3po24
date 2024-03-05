/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: ompt
// RUN: env OMP_NUM_THREADS=2 %must-run --must:mpimode MPMD --must:openmp %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/Locks_double_init 2>&1 \
// RUN: | %filecheck-may-segfault %s

#include <mpi.h>
#include <omp.h>

auto main(int argc, char** argv) -> int
{
    auto threadlevel_provided = MPI_THREAD_SINGLE - 1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &threadlevel_provided);
    auto rank = -1;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    auto lock = omp_lock_t{};
    omp_init_lock(&lock);
    // CHECK: Error: from: {{.*}}: Lock is already initialized and must not be initialized again.
    omp_init_lock(&lock);

    omp_destroy_lock(&lock);
    MPI_Finalize();
}
