/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: HAVE_MPI_IBCAST

// RUN: %must-run-ddl %mpiexec-numproc-flag 4 %must-bin-dir/NoOverlapIBcast 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file noOverlapIBcast.c
 * A must overlap test.
 * Contains no errors.
 *
 * @author Joachim Protze
 */
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

#define COUNT 4

int main(int argc, char** argv)
{

    int i = 0, rank, size;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Status status[COUNT];
    MPI_Request request[COUNT];
    //Enough tasks ?
    if (size < 4) {
        printf("This test needs at least 4 processes!\n");
        MPI_Finalize();
        return 1;
    }
    for (i = 0; i < COUNT; i++) {
        request[i] = MPI_REQUEST_NULL;
    }

    int data[] = {rank, rank, rank, rank}, expecteddata[] = {0, 1, 2, 3};

    //Say hello
    printf("Hello, I am rank %i of %i processes.\n", rank, size);
    ;
    for (i = 0; i < COUNT; i++)
        MPI_Ibcast(data + i, 1, MPI_INT, i, MPI_COMM_WORLD, request + i);

    MPI_Waitall(COUNT, request, status);

    for (i = 0; i < COUNT; i++)
        if (data[i] != expecteddata[i])
            printf("Rank %i has data[%i]=%i, expects %i\n", rank, i, data[i], expecteddata[i]);
    MPI_Finalize();

    return 0;
}
