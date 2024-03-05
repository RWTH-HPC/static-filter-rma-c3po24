// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_waitsome \
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
 *  - MPI_Isend
 *  - MPI_Waitsome
 *  - MPI_Recv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: waitsome.c 319 2004-08-16 11:25:02Z rusbetti $  
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
    const int MSG_TAG_1 = 17;
    const int MSG_TAG_2 = 18;

    int size = -1;
    int rank = -1;
    int value = -1;
    int value2 = -1;
    int index[3] = {0, 0, 0};
    int outcount = -1;
    int i = 0;

    MPI_Status status;
    MPI_Status status2;
    MPI_Status status_arr[3];

    MPI_Request request_arr[3];

    printf("Waitsome with two valid requests and one REQUEST_NULL\n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        /* going to receive message */
        MPI_Recv(&value2, COUNT, MPI_INT, 1, MSG_TAG_1, MPI_COMM_WORLD, &status);
        assert(value2 == 19);
        MPI_Recv(&value2, COUNT, MPI_INT, 1, MSG_TAG_2, MPI_COMM_WORLD, &status2);
    }

    if (rank == 1) {
        /* going to send message */
        value = 19;
        MPI_Isend(&value, COUNT, MPI_INT, 0, MSG_TAG_1, MPI_COMM_WORLD, &request_arr[0]);
        MPI_Isend(&value, COUNT, MPI_INT, 0, MSG_TAG_2, MPI_COMM_WORLD, &request_arr[1]);
        request_arr[2] = MPI_REQUEST_NULL;
        MPI_Waitsome(3, request_arr, &outcount, index, status_arr);
        for (i = 0; i <= outcount - 1; i++) {
            printf(" index (of now freed request)= %d \n", index[i]);
        }
    }

    MPI_Finalize();

    return 0;
}
