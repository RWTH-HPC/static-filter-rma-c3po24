/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 1 %must-bin-dir/umpire_op-no-free \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are .* operations that are not freed when MPI_Finalize was issued}}

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) */

/* op-no-free.c -- construct some MPI_Ops without freeing them */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

/* construct multiple instances of same op to exercise more Umpire code... */
#define OP_COUNT 5

typedef struct {
    double real, imag;
} Complex;

void myProd(void* inp, void* inoutp, int* len, MPI_Datatype* dptr)
{
    int i;
    Complex c;
    Complex* in = (Complex*)inp;
    Complex* inout = (Complex*)inoutp;

    for (i = 0; i < *len; ++i) {
        c.real = inout->real * in->real - inout->imag * in->imag;
        c.imag = inout->real * in->imag + inout->imag * in->real;
        *inout = c;
        in++;
        inout++;
    }

    return;
}

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    MPI_Comm comm = MPI_COMM_WORLD;
    int i;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    MPI_Op newop[OP_COUNT];

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(comm);

    for (i = 0; i < OP_COUNT; i++)
        MPI_Op_create(myProd, 1, &newop[i]);

    MPI_Barrier(comm);

    printf("(%d) Finished normally\n", rank);
    MPI_Finalize();
    return 0;
}

/* EOF */
