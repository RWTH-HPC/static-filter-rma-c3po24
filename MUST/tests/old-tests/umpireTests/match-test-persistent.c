/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 3 \
// RUN: %must-bin-dir/umpire_match-test-persistent 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/* Creator: Tobias Hilbrich */

/* match-test-persistent.c -- tests matching of persistent comm.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    int buf[4];
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    MPI_Status status;
    MPI_Request reqs[4];

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    switch (rank) {
    case 0:
        MPI_Send_init(buf, 1, MPI_INT, 1, 123, MPI_COMM_WORLD, &(reqs[0]));
        MPI_Send_init(buf, 1, MPI_INT, 1, 333, MPI_COMM_WORLD, &(reqs[1]));
        MPI_Start(&(reqs[0]));
        MPI_Start(&(reqs[1]));

        MPI_Wait(&(reqs[0]), &status);
        MPI_Wait(&(reqs[1]), &status);

        MPI_Request_free(&(reqs[0]));
        MPI_Request_free(&(reqs[1]));
        break;

    case 1:
        MPI_Recv_init(&(buf[0]), 1, MPI_INT, MPI_ANY_SOURCE, 123, MPI_COMM_WORLD, &(reqs[0]));
        MPI_Recv_init(&(buf[1]), 1, MPI_INT, MPI_ANY_SOURCE, 333, MPI_COMM_WORLD, &(reqs[1]));
        MPI_Recv_init(&(buf[2]), 1, MPI_INT, MPI_ANY_SOURCE, 123, MPI_COMM_WORLD, &(reqs[2]));
        MPI_Recv_init(&(buf[3]), 1, MPI_INT, MPI_ANY_SOURCE, 333, MPI_COMM_WORLD, &(reqs[3]));
        MPI_Start(&(reqs[3]));
        MPI_Startall(3, reqs);

        MPI_Wait(&(reqs[0]), &status);
        MPI_Wait(&(reqs[1]), &status);
        MPI_Wait(&(reqs[2]), &status);
        MPI_Wait(&(reqs[3]), &status);

        MPI_Request_free(&(reqs[0]));
        MPI_Request_free(&(reqs[1]));
        MPI_Request_free(&(reqs[2]));
        MPI_Request_free(&(reqs[3]));
        break;

    case 2:
        MPI_Send_init(buf, 1, MPI_INT, 1, 123, MPI_COMM_WORLD, &(reqs[0]));
        MPI_Send_init(buf, 1, MPI_INT, 1, 333, MPI_COMM_WORLD, &(reqs[1]));
        MPI_Startall(2, reqs);

        MPI_Wait(&(reqs[0]), &status);
        MPI_Wait(&(reqs[1]), &status);

        MPI_Request_free(&(reqs[0]));
        MPI_Request_free(&(reqs[1]));
        break;
    }

    printf("(%d) Finished normally\n", rank);
    MPI_Finalize();
    return 0;
}

/* EOF */
