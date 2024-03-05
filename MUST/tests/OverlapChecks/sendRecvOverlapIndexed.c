/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/SendRecvOverlapIndexed 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error|ERROR|Warning|WARNING}}

/**
 * @file sendRecvOverlapIndexedBlock.c
 * A must overlap test.
 * MPI_Sendrecv with overlapping send and receive buffer.
 *
 * @author Joachim Protze
 */
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include "mustTest.h"

#ifdef HAVE_MPI_GET_ADDRESS
#define My_MPI_Address MPI_Get_address
#else
#define My_MPI_Address MPI_Address
#endif

typedef struct {
    double coords[3];
    int sector[3];
    double velocity[3];
    double spin[3];
    char charge;
    double radius;
    double mass;
} particle_info;

int main(int argc, char** argv)
{
    particle_info cloud[110];
    int i, rank, size;
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

    MPI_Datatype structtype, indexedtype;

    int blocklens[7] = {3, 3, 3, 3, 1, 1, 1};
    MPI_Datatype types[7] =
        {MPI_DOUBLE, MPI_INT, MPI_DOUBLE, MPI_DOUBLE, MPI_CHAR, MPI_DOUBLE, MPI_DOUBLE};
    MPI_Aint displs[7];
    My_MPI_Address(cloud[0].coords, displs + 0);
    My_MPI_Address(cloud[0].sector, displs + 1);
    My_MPI_Address(cloud[0].velocity, displs + 2);
    My_MPI_Address(cloud[0].spin, displs + 3);
    My_MPI_Address(&(cloud[0].charge), displs + 4);
    My_MPI_Address(&(cloud[0].radius), displs + 5);
    My_MPI_Address(&(cloud[0].mass), displs + 6);
    for (i = 6; i >= 0; i--)
        displs[i] -= displs[0];

    MPI_Type_struct(7, blocklens, displs, types, &structtype);
    int array_of_blocklens[8] = {3, 2, 1, 2, 4, 8, 1, 3};
    int array_of_displs[8] = {3, 13, 23, 34, 44, 55, 65, 76};
    MPI_Type_indexed(8, array_of_blocklens, array_of_displs, structtype, &indexedtype);
    MPI_Type_commit(&indexedtype);

    //Say hello
    printf("Hello, I am rank %i of %i processes.\n", rank, size);
    ;

    if (rank == 0)
        MPI_Sendrecv(
            cloud,
            1,
            indexedtype,
            1,
            42,
            cloud + 5,
            1,
            indexedtype,
            1,
            42,
            MPI_COMM_WORLD,
            &status);
    if (rank == 1)
        MPI_Sendrecv(
            cloud,
            1,
            indexedtype,
            0,
            42,
            cloud + 25,
            1,
            indexedtype,
            0,
            42,
            MPI_COMM_WORLD,
            &status);

    MPI_Finalize();

    return 0;
}
