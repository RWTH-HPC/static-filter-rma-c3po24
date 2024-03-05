/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 --must:layout \
// RUN: %builddir/tests/TypeMatch/MatchStructMismatchErrorlayout.xml \
// RUN: %must-bin-dir/MatchStructMismatchError 2>&1 \
// RUN: | %filecheck --check-prefix 'CHECK-LOCAL' %s

// RUN: %must-run %mpiexec-numproc-flag 2 --must:layout \
// RUN: %builddir/tests/TypeMatch/DMatchStructMismatchErrorlayout.xml \
// RUN: %must-bin-dir/DMatchStructMismatchError 2>&1 \
// RUN: | %filecheck --check-prefix 'CHECK-DISTRIBUTED' %s

// CHECK-LOCAL: [MUST-REPORT]{{.*A send and a receive operation use datatypes that do not match!}}

// CHECK-DISTRIBUTED: [MUST-REPORT]{{.*A send and a receive operation use datatypes that do not match!}}

/**
 * @file MatchStructMismatchError.c
 * Type matching test with no error.
 *
 * Description:
 * A single send-recv match, the send and reveice both use a struct type,
 * type signature mismatch (Error).
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
    int blocklengths[3] = {3, 2, 1};
    MPI_Aint displacements[3] = {3 * sizeof(long), 6 * sizeof(long), 9 * sizeof(long)};
    MPI_Datatype oldTypes[3] = {MPI_LONG, MPI_LONG, MPI_LONG};

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
    if (rank == 0)
        oldTypes[2] = MPI_INT; //ERROR: must be MPI_LONG
    MPI_Type_struct(3, blocklengths, displacements, oldTypes, &newType);
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
