/* -*- Mode: C; -*- */

// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/umpire_persistent \
// RUN: 2>&1 \
// RUN: | %filecheck-may-segfault %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Waitall@.*.: Argument 2 [(]array_of_requests[)] has to be an array of predefined or user defined requests, the following entries are unknown requests:}}

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Thu Nov 30 2000 */
/* persistent.c -- do some MPI persistent calls */
/* including freeing an active request - the effect of which */
/* is implementation dependent - usually a detected error */

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
    MPI_Request aReq[2], free_req;
    MPI_Status aStatus[2];

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
        if (rank == 0) {
            memset(buf0, 0, buf_size);

            MPI_Send_init(buf0, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD, &aReq[0]);
            MPI_Recv_init(buf1, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD, &aReq[1]);

            MPI_Start(&aReq[0]);
            MPI_Start(&aReq[1]);

            MPI_Waitall(2, aReq, aStatus);

            memset(buf0, 1, buf_size);

            MPI_Startall(2, aReq);

            /* free an active request... */
            free_req = aReq[1];
            MPI_Request_free(&free_req);

            MPI_Waitall(2, aReq, aStatus);
        } else if (rank == 1) {
            memset(buf1, 1, buf_size);

            MPI_Recv_init(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD, &aReq[0]);
            MPI_Send_init(buf1, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD, &aReq[1]);

            MPI_Start(&aReq[0]);
            MPI_Start(&aReq[1]);

            MPI_Waitall(2, aReq, aStatus);

            memset(buf1, 0, buf_size);

            MPI_Startall(2, aReq);

            /* free an active request... */
            free_req = aReq[1];
            MPI_Request_free(&free_req);

            MPI_Waitall(2, aReq, aStatus);
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Request_free(&aReq[0]);

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */
