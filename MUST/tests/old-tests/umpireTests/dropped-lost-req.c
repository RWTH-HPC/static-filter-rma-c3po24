/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/umpire_dropped-lost-req 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Irecv@.*1: The application fails to match a point-to-point operation}}

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Wed Nov 29 2000 */
/* dropped-lost-req.c -- create a request that's never matched or completed */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>
#include <unistd.h>
#include <assert.h>

#define buf_size 128

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    int tag1 = 31;
    int tag2 = 32;
    MPI_Comm comm = MPI_COMM_WORLD;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    int buf0[buf_size];
    int buf1[buf_size];
    MPI_Request req0, req1;
    MPI_Status status1;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &nprocs);

    assert(nprocs > 1);

    MPI_Comm_rank(comm, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    memset(buf0, 1, buf_size);
    memset(buf1, 2, buf_size);

    MPI_Barrier(comm);

    /* 0 sends 1 one message, 1 sets up two recieves */
    switch (rank) {
    case 0:
        MPI_Isend(buf1, buf_size, MPI_INT, 1, tag2, comm, &req1);
        MPI_Wait(&req1, &status1);
        break;

    case 1:
        MPI_Irecv(buf0, buf_size, MPI_INT, 0, tag1, comm, &req0);
        MPI_Irecv(buf1, buf_size, MPI_INT, 0, tag2, comm, &req1);

        MPI_Wait(&req1, &status1);
        break;

    default:
        /* do nothing */
        break;
    }

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */
