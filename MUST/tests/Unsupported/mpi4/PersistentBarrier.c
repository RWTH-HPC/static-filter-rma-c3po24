/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */
// REQUIRES: HAVE_MPI_BARRIER_INIT
// RUN: %must-run %mpiexec-numproc-flag 1 %must-bin-dir/PersistentBarrier \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Information:.*The following MPI functions were used but are not supported by MUST:[[:space:]].*}}

/**
 * @file PersistentBarrier.cpp
 *
 * Description:
 * Uses the MPI-4 feature persisten collectives, here a barrier.
 *
 *  @date 12.07.2022
 *  @author Felix Tomski
 */

#include <mpi.h>

int main(int argc, char* argv[])
{
    MPI_Request req;
    MPI_Init(&argc, &argv);

    MPI_Barrier_init(MPI_COMM_WORLD, MPI_INFO_NULL, &req);
    MPI_Start(&req);
    MPI_Wait(&req, MPI_STATUS_IGNORE);

    MPI_Finalize();
    return 0;
}
