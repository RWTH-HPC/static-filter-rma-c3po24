/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file allgathervs.c
 * A stress test for MPI_Allgatherv collective checking.
 *
 * @author Tobias Hilbrich
 * @data 12.02.2013
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

int main(int argc, char** argv)
{
    int rank, size, i, *sbuf, *rbuf, count = 1, *rcounts, *rdispls;
    unsigned long long numColls = 10000;
    struct timeval t1, t2, t3;

    MPI_Init(&argc, &argv);
    gettimeofday(&t1, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (getenv("MUST_TEST_NUM_ROUNDS") != NULL)
        numColls = strtoll(getenv("MUST_TEST_NUM_ROUNDS"), NULL, 0);

    if (getenv("MUST_TEST_COUNT") != NULL)
        count = strtoll(getenv("MUST_TEST_COUNT"), NULL, 0);

    if (rank == 0)
        printf("allgathervs@%d rounds=%lld count=%d\n", size, numColls, count);

    rbuf = (int*)malloc(sizeof(int) * size * count);
    sbuf = (int*)malloc(sizeof(int) * count);

    rcounts = (int*)malloc(sizeof(int) * size);
    rdispls = (int*)malloc(sizeof(int) * size);
    for (i = 0; i < size; i++) {
        rcounts[i] = count;
        rdispls[i] = count * i;
    }

    for (i = 0; i < numColls; i++) {
        MPI_Allgatherv(sbuf, count, MPI_INT, rbuf, rcounts, rdispls, MPI_INT, MPI_COMM_WORLD);
    }

    gettimeofday(&t2, NULL);
    MPI_Finalize();
    gettimeofday(&t3, NULL);

    if (rank == 0)
        printf(
            "PostInit-PreFin: %ld PostInit-PostFin: %ld\n",
            (t2.tv_sec * 1000000 + t2.tv_usec) - (t1.tv_sec * 1000000 + t1.tv_usec),
            (t3.tv_sec * 1000000 + t3.tv_usec) - (t1.tv_sec * 1000000 + t1.tv_usec));

    free(rbuf);
    free(sbuf);
    free(rcounts);
    free(rdispls);

    return 0;
}