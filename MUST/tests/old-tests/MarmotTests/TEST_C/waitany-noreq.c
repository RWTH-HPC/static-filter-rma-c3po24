// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_waitany-noreq 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Waitany@.*: Argument 1 [(]count[)] is zero, which is correct but unusual!}}

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
 *  - MPI_Wait
 *  - MPI_Waitany
 *  - MPI_Recv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: waitany-noreq.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

/*
  ** We have no requests in the array 
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
    int index = -1;

    MPI_Status status;
    MPI_Status status_arr[1];

    MPI_Request request;
    MPI_Request request_arr[1];

    printf("We call Waitany with empty request_array, count=0.\n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        /* going to receive message */
        MPI_Recv(&value2, COUNT, MPI_INT, 1, MSG_TAG, MPI_COMM_WORLD, &status);
        assert(value2 == 19);
    }

    if (rank == 1) {
        /* going to send message */
        value = 19;
        MPI_Isend(&value, COUNT, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD, &request);
        MPI_Wait(&request, &status);
        MPI_Waitany(0, request_arr, &index, status_arr);
        if (index != MPI_UNDEFINED) {
            printf(" index(of now freed request)= %d \n", index);
        }
    }

    MPI_Finalize();

    return 0;
}
