/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 4 --must:fanin 2 \
// RUN: %must-bin-dir/DnbcCollIreduceWaitAnyNoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file nbcCollIreduceWaitAnyNoError.c
 * A test with correct MPI_Ireduce and MPI_Waitany calls (No Error).
 *
 * Description:
 * Each process executes as many MPI_Ireduce calls as there are processes and then waits for any of them to complete.
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
    int rank, size;
    MPI_Status status;
    MPI_Request* requests = NULL;
    int i, index;
    int buf = 7, *sums = NULL;

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

    requests = malloc(sizeof(MPI_Request) * size);
    sums = malloc(sizeof(int) * size);

    for (i = 0; i < size; i++) {
        MPI_Ireduce(
            &buf,
            &(sums[i]),
            1,
            MPI_INT,
            MPI_SUM,
            i /*root*/,
            MPI_COMM_WORLD,
            &(requests[i]));
    }

    for (i = 0; i < size; i++) {
        MPI_Waitany(size, requests, &index, &status);
        printf("Rank %d completed MPI_Ireduce #%d.\n", rank, index);
    }

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    if (requests)
        free(requests);
    if (sums)
        free(sums);
    MPI_Finalize();

    return 0;
}
