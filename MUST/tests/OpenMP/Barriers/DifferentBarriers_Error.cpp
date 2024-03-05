/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file
 * A Test for OpenMPbarriers checks.
 *
 * The program is faulty.
 *
 * This is a test for the analysis group OpenMPbarriers.
 */

// REQUIRES: ompt
// RUN: env OMP_NUM_THREADS=4 %must-run --must:openmp --must:mpimode MPMD %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/DifferentBarriers_Error 2>&1 \
// RUN: | %filecheck --implicit-check-not 'BAD TERMINATION' %s

#include <cassert>
#include <iostream>
#include <mpi.h>
#include <omp.h>

auto main(int argc, char** argv) -> int
{
    int threadlevel_provided = MPI_THREAD_SINGLE - 1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &threadlevel_provided);
    int rank = -1;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

#pragma omp parallel
    {
        assert(omp_get_num_threads() > 1);
        // CHECK-DAG: Error: {{.*}}: Thread passes a different barrier than other threads of the same team.
        if (omp_get_thread_num() % 2 == 0) {
#pragma omp barrier
            std::cout << "Rank " << rank << " passed Barrier 1" << std::endl;
        } else {
#pragma omp barrier
            std::cout << "Rank " << rank << " passed Barrier 2" << std::endl;
        }
    }

    MPI_Finalize();
}
