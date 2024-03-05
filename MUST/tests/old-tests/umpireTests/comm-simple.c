/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/umpire_comm-simple \
// RUN: 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

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
    MPI_Comm newcomm;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(comm);

    {
        int color = rank % 2;
        int key = 1;
        int nrank;
        int nsize;
        int dat = 0;

        MPI_Comm_split(comm, color, key, &newcomm);

        MPI_Comm_size(newcomm, &nsize);
        MPI_Comm_rank(newcomm, &nrank);
        printf(
            "world task %s/%d/%d maps to new comm task %s/%d/%d\n",
            "comm",
            nprocs,
            rank,
            "newcomm",
            nsize,
            nrank);

        if (nrank == 0) {
            dat = 1000 + color;
        }

        MPI_Bcast(&dat, 1, MPI_INT, 0, newcomm);

        printf(
            "world task %s/%d/%d maps to new comm task %s/%d/%d --> %d\n",
            "comm",
            nprocs,
            rank,
            "newcomm",
            nsize,
            nrank,
            dat);
    }

    MPI_Barrier(comm);
    MPI_Comm_free(&newcomm);

    printf("(%d) Finished normally\n", rank);
    MPI_Finalize();
    return 0;
}

/* EOF */
