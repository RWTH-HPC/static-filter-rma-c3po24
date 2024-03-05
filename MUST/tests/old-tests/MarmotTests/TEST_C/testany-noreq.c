// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_testany-noreq 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Testany@.*: Argument . [(]count[)] is zero, which is correct but unusual!}}

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
 *  - MPI_Testany
 *  - MPI_Recv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: testany-noreq.c 997 2009-10-02 14:44:28Z hpczink $
 */

/*
  ** We have no requests in the array
  */

#include <stdio.h>
#include <assert.h>
/* #include <time.h>
 * use instead
 */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <unistd.h>
#endif
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
    int flag = 0;

    MPI_Status status;
    MPI_Status status_arr[1];
    MPI_Request request;
    MPI_Request request_arr[1];

    printf("We call Testany with empty request_arr, count=0.\n");
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
#ifdef _WIN32
        Sleep(1000);
#else
        sleep(1);
#endif
        MPI_Testany(0, request_arr, &index, &flag, status_arr);
        if (index != MPI_UNDEFINED)
            printf(" index(of now freed request)= %d \n", index);
    }

    MPI_Finalize();

    return 1;
}
