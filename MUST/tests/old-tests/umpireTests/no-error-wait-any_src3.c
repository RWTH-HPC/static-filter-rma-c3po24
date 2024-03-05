/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/umpire_no-error-wait-any_src3 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Fri Mar  17 2000 */
/* no-error.c -- do some MPI calls without any errors */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#define buf_size 128
#define NUMREPS 3

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    int buf0[buf_size];
    int buf1[buf_size];
    int i;
    MPI_Status status;
    MPI_Request req;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(MPI_COMM_WORLD);

    if (NUMREPS < 1) {
        printf("not enough repetitions\n");
    }

    if (nprocs < 2) {
        printf("not enough tasks\n");
    } else if (rank == 0) {
        memset(buf0, 0, buf_size);

        MPI_Irecv(buf1, buf_size, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &req);

        MPI_Send(buf0, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD);

        for (i = 0; i < NUMREPS; i++) {
            MPI_Send(buf0, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD);

            MPI_Recv(buf0, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD, &status);
        }

        MPI_Wait(&req, &status);
    } else if (rank == 1) {
        MPI_Irecv(buf1, buf_size, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &req);

        MPI_Recv(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);

        MPI_Send(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);

        for (i = 0; i < NUMREPS - 1; i++) {
            MPI_Send(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);

            MPI_Recv(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
        }

        MPI_Send(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);

        MPI_Wait(&req, &status);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */
