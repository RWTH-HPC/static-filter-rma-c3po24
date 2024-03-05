// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_pers_requests1 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

/**
 *  @file
 *
 *  This program is a very simple MPI program to verify that the 
 *  library works correctly.
 *
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Finalize 
 *  - MPI_Comm_rank
 *  - MPI_Comm_size
 *  - MPI_Send_init
 *  - MPI_Start
 *  - MPI_Wait
 *  - MPI_Recv
 *  - MPI_Request_free
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: pers_requests1.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

/*
  ** We have 3 requests in the array - one is reused (but freed before),
  ** one usual and one which is non-valid
  */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    const int COUNT = 1;
    const int MSG_TAG = 17;

    int size = -1;
    int rank = -1;
    int value = 37;
    int value2 = -1;

    MPI_Status status;
    MPI_Request request;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        /* going to receive message */
        MPI_Recv(&value2, COUNT, MPI_INT, 1, MSG_TAG, MPI_COMM_WORLD, &status);
        assert(value2 == 37);
    }

    if (rank == 1) {
        /* going to send message */
        MPI_Send_init(&value, COUNT, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD, &request);
        MPI_Start(&request);
        MPI_Wait(&request, &status);
        MPI_Request_free(&request);
    }

    MPI_Finalize();

    return 0;
}
