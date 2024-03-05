/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/umpire_errhandler-no-free 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are 5 error handlers that are not freed when MPI_Finalize was issued}}

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) */

/* errhandler-no-error.c -- construct some MPI_Errhandlers and free them */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

/* multiple instances of same errhandler to exercise more Umpire code... */
#define ERRHANDLER_COUNT 5

void myErrhandler(MPI_Comm* comm, int* errorcode, ...)
{
    char buf[MPI_MAX_ERROR_STRING];
    int error_strlen;

    /* print alert */
    fprintf(stderr, "Caught an MPI Error! Time to abort!\n");

    /* get and print MPI error message... */
    MPI_Error_string(*(errorcode), buf, &error_strlen);
    fprintf(stderr, "%s\n", buf);

    MPI_Abort(*comm, *errorcode);

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
    MPI_Errhandler newerrhandler[ERRHANDLER_COUNT];

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(comm);

    for (i = 0; i < ERRHANDLER_COUNT; i++)
        MPI_Errhandler_create(myErrhandler, &newerrhandler[i]);

    MPI_Barrier(comm);

    printf("(%d) Finished normally\n", rank);
    MPI_Finalize();
    return 0;
}

/* EOF */
