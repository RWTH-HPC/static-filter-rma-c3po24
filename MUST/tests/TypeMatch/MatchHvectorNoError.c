/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 --must:layout \
// RUN: %builddir/tests/TypeMatch/MatchHvectorNoErrorlayout.xml \
// RUN: %must-bin-dir/MatchHvectorNoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

// RUN: %must-run %mpiexec-numproc-flag 2 --must:layout \
// RUN: %builddir/tests/TypeMatch/DMatchHvectorNoErrorlayout.xml \
// RUN: %must-bin-dir/DMatchHvectorNoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file MatchHvectorNoError.c
 * Type matching test with no error.
 *
 * Description:
 * A single send-recv match, the send and reveice both use a hvector type,
 * the spanned typesignatues match (No Error).
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>
#include "mustTest.h"

int main(int argc, char** argv)
{
    int rank, size;
    long data[100];
    MPI_Status status;
    MPI_Datatype newType;

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

    //Create a conti type
    MPI_Type_hvector(3, 3, 6 * sizeof(long), MPI_LONG, &newType);
    MPI_Type_commit(&newType);

    if (rank == 0)
        MPI_Send(data, 2, newType, 1, 666, MPI_COMM_WORLD);

    if (rank == 1)
        MPI_Recv(data, 2, newType, 0, 666, MPI_COMM_WORLD, &status);

    MPI_Type_free(&newType);
    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
