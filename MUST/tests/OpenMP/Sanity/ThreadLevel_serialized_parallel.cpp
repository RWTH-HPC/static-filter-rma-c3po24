/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file
 * A test for usage of threading level MPI_THREAD_SERIALIZED.
 *
 * Requests the threading level MPI_THREAD_SERIALIZED and correctly performs all MPI calls in
 * critical sections.
 *
 * This is a test for the analysis group OpenMPsanity.
 */

// REQUIRES: ompt
// RUN: env OMP_NUM_THREADS=4 %must-run --must:mpimode MPMD --must:openmp %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/ThreadLevel_serialized_parallel 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' --implicit-check-not 'BAD TERMINATION' %s

#include <mpi.h>

int main(int argc, char** argv)
{
    int provided = -1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_SERIALIZED, &provided);

#pragma omp parallel
    {
#pragma omp critical
        {
            int size = -1;
            MPI_Comm_size(MPI_COMM_WORLD, &size);
            int rank = -1;
            MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        }
    }

    MPI_Finalize();
    return 0;
}
