/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file ibarriers.c
 * A stress test to evaluate tool behavior for a loop that exibits
 * MPI_Ibarrier (and MPI_Wait) operations in a loop.
 *
 * @author Tobias Hilbrich
 * @data 18.08.2015
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

int main(int argc, char** argv)
{
    int rank, size, i;
    unsigned long long numColls = 10000;
    struct timeval t0, t1, t2, t3;
    MPI_Request request;
    MPI_Status status;

    if (getenv("MUST_TEST_NUM_ROUNDS") != NULL)
        numColls = strtoll(getenv("MUST_TEST_NUM_ROUNDS"), NULL, 0);

    gettimeofday(&t0, NULL);
    MPI_Init(&argc, &argv);
    gettimeofday(&t1, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (rank == 0)
        printf("ibarriers@%d rounds=%lld\n", size, numColls);

    for (i = 0; i < numColls; i++) {
        MPI_Ibarrier(MPI_COMM_WORLD, &request);
        MPI_Wait(&request, &status);
    }

    gettimeofday(&t2, NULL);
    MPI_Finalize();
    gettimeofday(&t3, NULL);

    if (rank == 0)
        printf(
            "PreInit-PostFin: %ld\n PostInit-PreFin: %ld\n PostInit-PostFin: %ld\n",
            (t3.tv_sec * 1000000 + t3.tv_usec) - (t0.tv_sec * 1000000 + t0.tv_usec),
            (t2.tv_sec * 1000000 + t2.tv_usec) - (t1.tv_sec * 1000000 + t1.tv_usec),
            (t3.tv_sec * 1000000 + t3.tv_usec) - (t1.tv_sec * 1000000 + t1.tv_usec));

    return 0;
}
