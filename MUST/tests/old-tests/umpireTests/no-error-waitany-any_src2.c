/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 3 \
// RUN: %must-bin-dir/umpire_no-error-waitany-any_src2 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Fri Mar  17 2000 */
/* no-error.c -- do some MPI calls without any errors */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>
#include <assert.h>

#define buf_size 128

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    int done;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    int buf0[buf_size];
    int buf1[buf_size];
    int buf2[buf_size];
    int buf3[buf_size];
    MPI_Status status;
    MPI_Request reqs[2];

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(MPI_COMM_WORLD);

    /* this code is very similar to any_src-waitall-deadlock.c */
    /* but eliminates the deadlock by using MPI_Waitany... */
    /* also very similar to no-error-waitall-any_src.c but */
    /* this code is deterministic; buf0 holds result of */
    /* memset to 0 and buf1 holds the result of memset to */
    /* in all three tasks since must match the first Irecv first... */
    if (nprocs < 3) {
        printf("not enough tasks\n");
    } else if (rank == 0) {
        MPI_Irecv(buf0, buf_size, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &reqs[0]);

        MPI_Irecv(buf1, buf_size, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &reqs[1]);

        MPI_Waitany(2, reqs, &done, &status);

        assert(done == 0);

        MPI_Send(buf2, buf_size, MPI_INT, 1, 1, MPI_COMM_WORLD);

        MPI_Send(buf3, buf_size, MPI_INT, 2, 2, MPI_COMM_WORLD);

        MPI_Wait(&reqs[1], &status);
    } else if (rank == 1) {
        memset(buf0, 0, buf_size);

        MPI_Send(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);

        MPI_Recv(buf1, buf_size, MPI_INT, 0, 1, MPI_COMM_WORLD, &status);
    } else if (rank == 2) {
        MPI_Recv(buf0, buf_size, MPI_INT, 0, 2, MPI_COMM_WORLD, &status);

        memset(buf1, 1, buf_size);

        MPI_Send(buf1, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */
