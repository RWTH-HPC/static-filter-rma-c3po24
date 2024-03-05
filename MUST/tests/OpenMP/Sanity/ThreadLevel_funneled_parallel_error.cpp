/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file
 * A Test for threading level MPI_THREAD_FUNNELED.
 *
 * Requests the threading level MPI_THREAD_FUNNELED and performs MPI calls incorrectly also from
 * other threads than the main thread.
 *
 * This is a test for the analysis group OpenMPsanity.
 */

// REQUIRES: ompt
// RUN: env OMP_NUM_THREADS=4 %must-run --must:mpimode MPMD --must:openmp %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/ThreadLevel_funneled_parallel_error 2>&1 \
// RUN: | %filecheck --implicit-check-not 'BAD TERMINATION' %s

#include <mpi.h>

int main(int argc, char** argv)
{
    int provided = -1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_FUNNELED, &provided);

#pragma omp parallel
    {
        int size = -1;
        // CHECK-DAG: Error: from: call MPI_Comm_size@{{.*}}: The MPI thread support level permits executing MPI calls on the main thread only.
        MPI_Comm_size(MPI_COMM_WORLD, &size);
        int rank = -1;
        // CHECK-DAG: Error: from: call MPI_Comm_rank@{{.*}}: The MPI thread support level permits executing MPI calls on the main thread only.
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    }

    MPI_Finalize();
    return 0;
}
