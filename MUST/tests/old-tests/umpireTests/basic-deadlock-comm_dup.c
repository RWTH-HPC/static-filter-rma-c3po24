/* -*- Mode: C; -*- */
// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/umpire_basic-deadlock-comm_dup 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-RUNTIME] ERROR: MUST detected a deadlock, detailed information is available in the MUST output file.

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Fri Mar  17 2000 */
/* no-error.c -- do some MPI calls without any errors */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#define buf_size 128

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    int buf0[buf_size];
    int buf1[buf_size];
    MPI_Status status;
    MPI_Comm comm;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(MPI_COMM_WORLD);

    if (nprocs < 2) {
        printf("not enough tasks\n");
    } else {
        MPI_Comm_dup(MPI_COMM_WORLD, &comm);

        if (rank == 0) {
            memset(buf0, 0, buf_size);

            MPI_Recv(buf1, buf_size, MPI_INT, 1, 0, comm, &status);

            MPI_Send(buf0, buf_size, MPI_INT, 1, 0, comm);
        } else if (rank == 1) {
            memset(buf1, 1, buf_size);

            MPI_Recv(buf0, buf_size, MPI_INT, 0, 0, comm, &status);

            MPI_Send(buf1, buf_size, MPI_INT, 0, 0, comm);
        }

        MPI_Comm_free(&comm);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */