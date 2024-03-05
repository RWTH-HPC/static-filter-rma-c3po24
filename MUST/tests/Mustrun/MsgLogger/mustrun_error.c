/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %not %must-run-errorcode --must:output html %mpiexec-numproc-flag 2 %must-bin-dir/mustrun_msglogger_error
// RUN: %not %must-run-errorcode --must:output stdout %mpiexec-numproc-flag 2 %must-bin-dir/mustrun_msglogger_error
// RUN: %not %must-run-errorcode --must:output json %mpiexec-numproc-flag 2 %must-bin-dir/mustrun_msglogger_error

/**
 * @file mustrun_error.c
 * Error dummy to test return code of different output modes.
 * Same as LeakCommError from LeakChecks.
 *
 *  @date 19.05.2011
 *  @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>

int main(int argc, char** argv)
{
    int size, rank;

    MPI_Group group;
    MPI_Comm comm;

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

    //Do the testing
    MPI_Comm_group(MPI_COMM_WORLD, &group);
    MPI_Comm_create(MPI_COMM_WORLD, group, &comm);
    MPI_Group_free(&group);
    /*MISSING: MPI_Comm_free (&comm); */

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
