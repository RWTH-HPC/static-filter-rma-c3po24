/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 1 \
// RUN: %must-bin-dir/umpire_type-no-free-exhaustive 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are .* datatypes that are not freed when MPI_Finalize was issued}}

/* Creator: Bronis R. de Supinski (bronis@llnl.gov)  */

/* type-no-free-exhaustive.c -- use all type constructors without freeing */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#define TYPE_CONSTRUCTOR_COUNT 6

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    MPI_Comm comm = MPI_COMM_WORLD;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    int blocklens[2];
    MPI_Aint displs[2];
    int idispls[2];
    MPI_Datatype newtype[TYPE_CONSTRUCTOR_COUNT];
    MPI_Datatype newtype2[TYPE_CONSTRUCTOR_COUNT];

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    newtype2[0] = MPI_DOUBLE;
    newtype2[1] = MPI_CHAR;
    blocklens[0] = blocklens[1] = 1;
    displs[0] = idispls[0] = 0;
    displs[1] = idispls[1] = 8;

    MPI_Barrier(comm);

    /* create the types */
    MPI_Type_struct(2, blocklens, displs, newtype2, &newtype[0]);
    MPI_Type_vector(2, 3, 4, newtype[0], &newtype[1]);
    MPI_Type_hvector(3, 2, 192, newtype[1], &newtype[2]);
    displs[1] = 2;
    MPI_Type_indexed(2, blocklens, idispls, newtype[2], &newtype[3]);
    displs[1] = 512;
    MPI_Type_hindexed(2, blocklens, displs, newtype[3], &newtype[4]);
    displs[1] = 8;
    MPI_Type_contiguous(10, newtype[4], &newtype[5]);

    MPI_Barrier(comm);

    printf("(%d) Finished normally\n", rank);
    MPI_Finalize();
    return 0;
}

/* EOF */
