/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: HAVE_MPI_IREDUCE

// RUN: %must-run-ddl %mpiexec-numproc-flag 4 %must-bin-dir/NoOverlapIReduce 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file noOverlapIReduce.c
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

    int redrank = size * (size - 1) / 2;
    int data[] = {rank, rank, rank, rank}, reddata = 0;

    //Say hello
    printf(
        "Hello, I am rank %i of %i processes: {%i, %i, %i, %i}\n",
        rank,
        size,
        data[0],
        data[1],
        data[2],
        data[3]);
    for (i = 0; i < COUNT; i++)
        MPI_Ireduce(data + i, &reddata, 1, MPI_INT, MPI_SUM, i, MPI_COMM_WORLD, request + i);

    MPI_Waitall(COUNT, request, status);

    if (rank < 4)
        if (reddata != redrank)
            printf("Rank %i has reddata=%i, expects %i\n", rank, reddata, redrank);
    MPI_Finalize();

    return 0;
}
