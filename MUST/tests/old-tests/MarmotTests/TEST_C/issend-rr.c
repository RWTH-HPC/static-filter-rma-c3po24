// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_issend-rr \
// RUN: 2>&1 \
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
 *  - MPI_Issend
 *  - MPI_Wait
 *  - MPI_Irecv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: issend-rr.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

/* 
** Here we re-use a request we didn't free before
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
    int value = -1;
    int value2 = -1;

    MPI_Status send_status;
    MPI_Status status;

    MPI_Request send_request;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        /* going to receive the message and reuse a non-freed request */
        MPI_Recv(&value, COUNT, MPI_INT, 1, MSG_TAG, MPI_COMM_WORLD, &status);
        assert(value == 19);
    }

    if (rank == 1) {
        value2 = 19;
        /* this is just to use the request */
        /*MPI_Isend(&value,COUNT,MPI_INT,0,18,MPI_COMM_WORLD,&send_request); */

        /* going to send the message */
        MPI_Issend(&value2, COUNT, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD, &send_request);
        MPI_Wait(&send_request, &send_status);
    }

    MPI_Finalize();

    return 0;
}
