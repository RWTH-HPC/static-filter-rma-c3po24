/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: ompt
// XFAIL: clang-15

// libomp triggers an assertion before invoking the callback if built without NDEBUG
// See: https://git-ce.rwth-aachen.de/hpc-research/correctness/MUST/-/issues/109
// UNSUPPORTED: icc

// RUN: env OMP_NUM_THREADS=4 %must-run --must:mpimode MPMD --must:openmp %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/Locks_uninitialized 2>&1 \
// RUN: | %filecheck-may-segfault %s

#include <mpi.h>
#include <omp.h>

auto main(int argc, char** argv) -> int
{
    int threadlevel_provided = MPI_THREAD_SINGLE - 1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &threadlevel_provided);
    int rank = -1;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    auto lock = omp_lock_t{};

#pragma omp parallel
    {
        // CHECK: Error: {{.*}}: Operating on an uninitialized lock.
        omp_set_lock(&lock);
        omp_unset_lock(&lock);
    }

    omp_destroy_lock(&lock);
    MPI_Finalize();
}
