/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 3 \
// RUN: %must-bin-dir/umpire_comm-bcast-deadlock 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-RUNTIME] ERROR: MUST detected a deadlock, detailed information is available in the MUST output file.

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Tue Aug 12 2003 */

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
    MPI_Comm inverted_comm;
    int bcast_rank;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(MPI_COMM_WORLD);

    if (nprocs != 3) {
        printf("Incorrect number of tasks; exactly 3 required\n");
    } else {
        /* create inverted communicator... */
        MPI_Comm_split(comm, 0, nprocs - rank, &inverted_comm);

        if (rank == 1) {
            MPI_Bcast(&rank, 1, MPI_INT, 1, inverted_comm);
            MPI_Bcast(&bcast_rank, 1, MPI_INT, 2, comm);
        } else if (rank == 2) {
            MPI_Bcast(&rank, 1, MPI_INT, 2, comm);
            MPI_Bcast(&bcast_rank, 1, MPI_INT, 1, inverted_comm);
        } else {
            MPI_Bcast(&bcast_rank, 1, MPI_INT, 2, comm);
            MPI_Bcast(&bcast_rank, 1, MPI_INT, 1, inverted_comm);
        }
    }

    MPI_Barrier(comm);

    printf("(%d) Finished normally\n", rank);

    MPI_Finalize();
    return 0;
}

/* EOF */
