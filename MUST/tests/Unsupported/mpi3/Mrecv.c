/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/UnsupportedMrecv \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Information:.*The following MPI functions were used but are not supported by MUST:[[:space:]].*}}

/**
 * @file Mrecv.cpp
 *
 * Description:
 * Uses the MUST-unsupported functions Mprobe and Mrecv.
 *
 *  @date 23.03.2023
 *  @author Felix Tomski
 */

#include <mpi.h>

int main(int argc, char* argv[])
{
    int rank, buf;
    MPI_Message msg;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (rank == 0) {
        MPI_Send(&buf, 1, MPI_INT, 1, 42, MPI_COMM_WORLD);
    }

    if (rank == 1) {
        MPI_Mprobe(0, 42, MPI_COMM_WORLD, &msg, MPI_STATUS_IGNORE);
        MPI_Mrecv(&buf, 1, MPI_INT, &msg, MPI_STATUS_IGNORE);
    }

    MPI_Finalize();
    return 0;
}
