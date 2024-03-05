/* -*- Mode: C; -*- */
// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/umpire_finalize-deadlock 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-RUNTIME] ERROR: MUST detected a deadlock, detailed information is available in the MUST output file.

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Fri Mar  17 2000 */
/* finalize-deadlock.c -- deadlock caused by calling MPI_Finalize */

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
    int its_raining = 0;
    MPI_Status status;

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
    } else if (rank == 0) {
        MPI_Recv(buf0, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD, &status);
    } else if ((rank == 1) && (its_raining)) {
        /* This code is never executed... */
        memset(buf0, 1, buf_size);

        MPI_Send(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);
    }

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */
