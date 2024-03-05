/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/SendRecvOverlap 2>&1 \
// RUN: | %filecheck %s

// CHECK-DAG: [MUST-REPORT] Error: from: call MPI_Sendrecv@0: The memory regions spanned by the recv part overlaps at the 0(th) repetition of datatype at its position (MPI_INT) with regions spanned by the send part of this operation!
// CHECK-DAG: [MUST-REPORT] Error: from: call MPI_Sendrecv@1: The memory regions spanned by the recv part overlaps at the 0(th) repetition of datatype at its position (MPI_INT) with regions spanned by the send part of this operation!

/**
 * @file sendRecvOverlap.c
 * A must overlap test.
 * MPI_Sendrecv with overlapping send and receive buffer.
 *
 * @author Joachim Protze
 */
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include "mustTest.h"

#define COUNT 4

int main(int argc, char** argv)
{

    int rank, size;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Status status;

    //Enough tasks ?
    if (size < 2) {
        printf("This test needs at least 2 processes!\n");
        MPI_Finalize();
        return 1;
    }

    MPI_Datatype vectortype, structtype;

    MPI_Type_vector(10, 1, 5, MPI_INT, &vectortype);

    MPI_Aint displs[3] = {0, 0, 4};

    MPI_Type_extent(MPI_INT, displs + 2);

    int* inbuf = malloc(100000);

#ifdef HAVE_MPI_TYPE_CREATE_RESIZED
    MPI_Type_create_resized(vectortype, displs[0], displs[2], &structtype);
#else
    int blocklens[3] = {1, 1, 1};
    MPI_Datatype types[3] = {MPI_LB, vectortype, MPI_UB};
    MPI_Type_struct(3, blocklens, displs, types, &structtype);
#endif

    MPI_Type_commit(&structtype);

    //Say hello
    printf("Hello, I am rank %i of %i processes.\n", rank, size);
    ;

    if (rank == 0)
        MPI_Sendrecv(inbuf, 3, MPI_INT, 1, 42, inbuf, 2, MPI_INT, 1, 42, MPI_COMM_WORLD, &status);
    if (rank == 1)
        MPI_Sendrecv(inbuf, 2, MPI_INT, 0, 42, inbuf, 3, MPI_INT, 0, 42, MPI_COMM_WORLD, &status);

    //     MPI_Startall(COUNT,request);
    MPI_Type_free(&vectortype);
    MPI_Type_free(&structtype);
    free(inbuf);

    MPI_Finalize();

    return 0;
}
