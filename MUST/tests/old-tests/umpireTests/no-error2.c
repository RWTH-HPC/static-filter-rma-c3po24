/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/umpire_no-error2 \
// RUN: 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Fri Mar  17 2000 */
/* no-error2.c -- do some MPI calls without any errors */
/* unlike no-error.c, this uses a raing and calls MPI_Barrier... */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#define buf_size 128

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    int src, dest;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    int buf0[buf_size];
    int buf1[buf_size];
    MPI_Status status;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    src = (rank + 1) % nprocs;
    dest = (rank - 1 + nprocs) % nprocs;

    memset(buf0, 0, buf_size);

    MPI_Sendrecv(
        buf0,
        buf_size,
        MPI_INT,
        dest,
        0,
        buf1,
        buf_size,
        MPI_INT,
        src,
        0,
        MPI_COMM_WORLD,
        &status);

    memset(buf1, 1, buf_size);

    MPI_Sendrecv(
        buf1,
        buf_size,
        MPI_INT,
        src,
        0,
        buf0,
        buf_size,
        MPI_INT,
        dest,
        0,
        MPI_COMM_WORLD,
        &status);

    memset(buf0, 0, buf_size);

    MPI_Sendrecv(
        buf0,
        buf_size,
        MPI_INT,
        dest,
        0,
        buf1,
        buf_size,
        MPI_INT,
        src,
        0,
        MPI_COMM_WORLD,
        &status);

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */
