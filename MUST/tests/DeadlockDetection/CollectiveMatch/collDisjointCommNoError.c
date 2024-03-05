/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 7 \
// RUN: %must-bin-dir/collDisjointCommNoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

// RUN: %must-run %mpiexec-numproc-flag 7 --must:layout \
// RUN: %builddir/tests/DeadlockDetection/DCollectiveMatch/collDisjointCommNoErrorlayout.xml \
// RUN: %must-bin-dir/DcollDisjointCommNoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file collInverseCommNoError.c
 * A test wich uses a user defined communicator and issues correct collectives (No Error).
 *
 * Description:
 * A new communicator that splits MPI_COMM_WORLD into odd and even ranks is used.
 * On each comm different collectives are issued.
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
    int rank, size, i;
    MPI_Comm csplit;

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

    //== Create splited odd/even comm
    MPI_Comm_split(MPI_COMM_WORLD, rank % 2, size - rank, &csplit);
    MPI_Barrier(MPI_COMM_WORLD);
    //Do different things on each comm
    if (rank % 2) {
        for (i = 0; i < 5; i++) {
            MPI_Bcast(&size, 1, MPI_INT, 0, csplit);
            MPI_Barrier(csplit);
        }
    } else {
        for (i = 0; i < 5; i++) {
            MPI_Barrier(csplit);
            MPI_Bcast(&size, 1, MPI_INT, 0, csplit);
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    //Clean up
    MPI_Comm_free(&csplit);

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
