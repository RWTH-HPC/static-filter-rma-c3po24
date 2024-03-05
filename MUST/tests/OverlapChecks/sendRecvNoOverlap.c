/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/SendRecvNoOverlap \
// RUN: 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

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

    char* inbuf = malloc(100000);
    MPI_Datatype vectortype, structtype;

    int blocklens[4] = {1, 2, 3, 4};
    MPI_Aint displs[4] = {-7, 0, 16, 64};

#ifdef HAVE_MPI_TYPE_CREATE_RESIZED
    {
        MPI_Datatype types[3] = {MPI_INT, MPI_DOUBLE, MPI_BYTE};
        MPI_Datatype temptype;
        MPI_Type_struct(3, blocklens + 1, displs + 1, types, &temptype);
        MPI_Aint lb, extent;
        MPI_Type_get_extent(temptype, &lb, &extent);
        MPI_Type_create_resized(temptype, displs[0], extent, &structtype);
        MPI_Type_free(&temptype);
    }
#else
    {
        MPI_Datatype types[4] = {MPI_LB, MPI_INT, MPI_DOUBLE, MPI_BYTE};
        MPI_Type_struct(4, blocklens, displs, types, &structtype);
    }
#endif
    MPI_Type_vector(5, 1, 4, structtype, &vectortype);

    MPI_Type_commit(&vectortype);
    MPI_Aint extent;
    MPI_Type_extent(structtype, &extent);

    //Say hello
    printf("Hello, I am rank %i of %i processes.\n", rank, size);
    ;

    if (rank == 0)
        MPI_Sendrecv(
            inbuf,
            3,
            vectortype,
            1,
            42,
            inbuf + extent * 2,
            2,
            vectortype,
            1,
            42,
            MPI_COMM_WORLD,
            &status);
    if (rank == 1)
        MPI_Sendrecv(
            inbuf + extent * 2,
            2,
            vectortype,
            0,
            42,
            inbuf,
            3,
            vectortype,
            0,
            42,
            MPI_COMM_WORLD,
            &status);

    //     MPI_Startall(COUNT,request);
    MPI_Type_free(&vectortype);
    MPI_Type_free(&structtype);
    free(inbuf);

    MPI_Finalize();

    return 0;
}
