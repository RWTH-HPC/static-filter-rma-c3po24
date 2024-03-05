/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file
 * A test for usage of threading level MPI_THREAD_SINGLE.
 *
 * Requests the threading level MPI_THREAD_SINGLE and incorrectly makes use of threads.
 *
 * This is a test for the analysis group OpenMPsanity.
 */

// REQUIRES: ompt
// RUN: env OMP_NUM_THREADS=4 %must-run --must:mpimode MPMD --must:openmp %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/ThreadLevel_single_parallel 2>&1 \
// RUN: | %filecheck %s

#include <iostream>
#include <mpi.h>
#include <omp.h>

int main(int argc, char** argv)
{
    int provided = -1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_SINGLE, &provided);
    // CHECK: Use of OpenMP parallel region prohibited: MPI initialized with 'MPI_THREAD_SINGLE'.
#pragma omp parallel
    {
        std::cout << "Thread " << omp_get_thread_num() << " says \"Hi!\"." << std::endl;
    }

    MPI_Finalize();
    return 0;
}
