/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file
 * Test for an emitted MUST runtime warning.
 *
 * Initializes the OpenMP runtime before MPI_Init_thread. This is currently not supported by MUST
 * and should result in a warning.
 *
 * This is a test for the OpenMPadapter module.
 */

// REQUIRES: ompt
// RUN: %must-run --must:mpimode MPMD --must:openmp %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/warn_omp_before_mpi_init 2>&1 \
// RUN: | %filecheck --implicit-check-not 'BAD TERMINATION' %s

#include <mpi.h>
#include <omp.h>

auto main(int argc, char** argv) -> int
{

    // CHECK: Warning: The OpenMP runtime has been initialized before the call to MPI_Init or MPI_Init_thread.
    omp_set_num_threads(2);

    int threadlevel_provided = MPI_THREAD_SINGLE - 1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &threadlevel_provided);

    MPI_Finalize();
}
