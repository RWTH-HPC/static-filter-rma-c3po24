/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/umpire_comm-deadlock \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-RUNTIME] ERROR: MUST detected a deadlock, detailed information is available in the MUST output file.

/* Creator: Jeffrey Vetter (vetter3@llnl.gov) Thu Feb 24 2000 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#define buf_size 128

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    MPI_Comm comm = MPI_COMM_WORLD;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    MPI_Comm nc1;
    int dat = 1234;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(comm);

    if (rank == 0) {
        printf("Creating first new comm\n");
    }
    {
        int color = rank % 2;
        int key = 1;
        int nrank;
        int nsize;
        MPI_Comm_split(comm, color, key, &nc1);
        MPI_Comm_size(nc1, &nsize);
        MPI_Comm_rank(nc1, &nrank);
        printf(
            "world task %s/%d/%d maps to new comm task %s/%d/%d\n",
            "comm",
            nprocs,
            rank,
            "nc1",
            nsize,
            nrank);
    }

    MPI_Barrier(comm);

    printf("Entering deadlock state.....\n");

    if (rank == 1) {
        MPI_Bcast(&dat, 1, MPI_INT, 0, nc1);
    } else {
        MPI_Bcast(&dat, 1, MPI_INT, 0, comm);
    }

    MPI_Barrier(comm);

    printf("(%d) Finished normally\n", rank);
    MPI_Finalize();
    return 0;
}

/* EOF */
