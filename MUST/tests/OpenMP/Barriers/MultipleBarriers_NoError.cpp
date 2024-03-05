/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file
 * A Test for OpenMPbarriers checks.
 *
 * The program is correct and stresses the tracking of active OpenMP barriers.
 *
 * This is a test for the analysis group OpenMPbarriers.
 */

// REQUIRES: ompt
// RUN: env OMP_NUM_THREADS=4,2 %must-run --must:openmp --must:mpimode MPMD %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/MultipleBarriers_NoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' --implicit-check-not 'BAD TERMINATION' %s

#include <mpi.h>

auto main(int argc, char** argv) -> int
{
    int threadlevel_provided = MPI_THREAD_SINGLE - 1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &threadlevel_provided);
    int rank = -1;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    // Multiple barriers with different codeptr_ra
#pragma omp parallel
    {
#pragma omp barrier
#pragma omp barrier
#pragma omp barrier
#pragma omp barrier
#pragma omp barrier
#pragma omp barrier
    }

    // The "same" barrier multiple times.
#pragma omp parallel
    {
        constexpr int iterations = 100;
        for (int i = 0; i < iterations; ++i) {
#pragma omp barrier
        }
    }

    // Nested parallel regions
#pragma omp parallel
    {
        constexpr int iterations = 100;
#pragma omp parallel
        for (int i = 0; i < iterations; ++i) {
#pragma omp barrier
        }
    }

    MPI_Finalize();
}
