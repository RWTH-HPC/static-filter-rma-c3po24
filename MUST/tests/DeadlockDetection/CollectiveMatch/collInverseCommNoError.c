/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 5 \
// RUN: %must-bin-dir/collInverseCommNoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

// RUN: %must-run %mpiexec-numproc-flag 5 --must:layout \
// RUN: %builddir/tests/DeadlockDetection/DCollectiveMatch/collInverseCommNoErrorlayout.xml \
// RUN: %must-bin-dir/DcollInverseCommNoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file collInverseCommNoError.c
 * A test wich uses a user defined communicator and issues correct collectives (No Error).
 *
 * Description:
 * A new communicator that reverses the order of MPI_COMM_WORLD is used in a Bcast and
 * a barrier, the same collectives are also executed on MPI_COMM_WORLD.
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
    int rank, size, *ranksIncl, i;
    MPI_Group gworld, ginverse;
    MPI_Comm cinverse;

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

    //== Create an inverse comm
    MPI_Comm_group(MPI_COMM_WORLD, &gworld);
    ranksIncl = (int*)malloc(sizeof(int) * size);
    for (i = 0; i < size; i++)
        ranksIncl[i] = size - i - 1;
    MPI_Group_incl(gworld, size, ranksIncl, &ginverse);
    MPI_Comm_create(MPI_COMM_WORLD, ginverse, &cinverse);

    //Do some colls on world and the inverse comm
    MPI_Bcast(&rank, 1, MPI_INT, 0, MPI_COMM_WORLD);
    MPI_Barrier(cinverse);
    MPI_Bcast(&rank, 1, MPI_INT, 0, cinverse);
    MPI_Barrier(MPI_COMM_WORLD);

    //Clean up
    MPI_Comm_free(&cinverse);
    MPI_Group_free(&ginverse);
    MPI_Group_free(&gworld);

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
