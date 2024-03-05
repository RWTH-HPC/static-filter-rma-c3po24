/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %not %must-run-errorcode --must:output html %mpiexec-numproc-flag 2 %must-bin-dir/mustrun_msglogger_error_timeout
// RUN: %not %must-run-errorcode --must:output stdout %mpiexec-numproc-flag 2 %must-bin-dir/mustrun_msglogger_error_timeout
// RUN: %not %must-run-errorcode --must:output json %mpiexec-numproc-flag 2 %must-bin-dir/mustrun_msglogger_error_timeout

// RUN: %not %must-run-ddl --must:no-errorcode --must:output html %mpiexec-numproc-flag 3 %must-bin-dir/mustrun_msglogger_error_timeout
// RUN: %not %must-run-ddl --must:no-errorcode --must:output stdout %mpiexec-numproc-flag 3 %must-bin-dir/mustrun_msglogger_error_timeout
// RUN: %not %must-run-ddl --must:no-errorcode --must:output json %mpiexec-numproc-flag 3 %must-bin-dir/mustrun_msglogger_error_timeout

/**
 * @file mustrun_error_timeout.c
 * Test whether the error code is properly propagated by MUST in case of timeout.
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>

int main(int argc, char** argv)
{
    int rank, size, buf1, buf2;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    //Enough tasks ?
    if (size < 2) {
        printf("This test needs at least 2 processes!\n");
        MPI_Finalize();
        return 1;
    }

    //Say hello
    printf("Hello, I am rank %d of %d processes.\n", rank, size);

    if (rank == 0) {
        MPI_Send(&buf1, 1, MPI_INT, 1, 666, MPI_COMM_WORLD);
        MPI_Recv(&buf2, 1, MPI_INT, 1, 666, MPI_COMM_WORLD, &status);
    }

    if (rank == 1) {
        MPI_Send(&buf1, 1, MPI_INT, 0, 666, MPI_COMM_WORLD);
        MPI_Recv(&buf2, 1, MPI_INT, 0, 666, MPI_COMM_WORLD, &status);
    }

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
