/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file barriers.c
 * A test for Euro MPI 2011 paper.
 *
 * @author Tobias Hilbrich
 * @data 6.5.2011
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
    int rank, size, i;
    unsigned long long numBarriers = 10000;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (getenv("MUST_TEST_NUM_COLLS") != NULL)
        numBarriers = strtoll(getenv("MUST_TEST_NUM_COLLS"), NULL, 0);

    if (rank == 0)
        printf("barriers@%d count=%llu\n", size, numBarriers);

    for (i = 0; i < numBarriers; i++) {
        MPI_Barrier(MPI_COMM_WORLD);
    }

    MPI_Finalize();
    return 0;
}
