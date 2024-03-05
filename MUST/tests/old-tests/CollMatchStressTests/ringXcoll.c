/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file simpleCollNoError.c
 * Simple test for collective matching.
 *
 * Description:
 * Each process issues an MPI_Barrier with MPI_COMM_WORLD (no error).
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

int main(int argc, char** argv)
{
    int rank, size, temp;
    MPI_Status status;
    MPI_Request request;
    int i;
    unsigned long long iters = 10000;
    struct timeval t1, t2, t3;

    MPI_Init(&argc, &argv);
    gettimeofday(&t1, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (getenv("MUST_TEST_NUM_ROUNDS") != NULL)
        iters = strtoll(getenv("MUST_TEST_NUM_ROUNDS"), NULL, 0);

    if (rank == 0)
        printf("ringXcoll@%d rounds=%lld X=10\n", size, iters);

    //Enough tasks ?
    if (size < 2) {
        printf("This test needs at least 2 processes!\n");
        MPI_Finalize();
        return 1;
    }

    for (i = 0; i < iters; i++) {
        if (i % 10 == 0) {
            MPI_Barrier(MPI_COMM_WORLD);
        }

        MPI_Isend(&size, 1, MPI_INT, (rank + 1) % size, i, MPI_COMM_WORLD, &request);
        MPI_Recv(&temp, 1, MPI_INT, ((rank - 1) + size) % size, i, MPI_COMM_WORLD, &status);
        MPI_Wait(&request, &status);
    }

    gettimeofday(&t2, NULL);
    MPI_Finalize();
    gettimeofday(&t3, NULL);

    if (rank == 0)
        printf(
            "PostInit-PreFin: %ld PostInit-PostFin: %ld\n",
            (t2.tv_sec * 1000000 + t2.tv_usec) - (t1.tv_sec * 1000000 + t1.tv_usec),
            (t3.tv_sec * 1000000 + t3.tv_usec) - (t1.tv_sec * 1000000 + t1.tv_usec));

    return 0;
}
