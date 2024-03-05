// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_testany-nvr2 2>&1 \
// RUN: | %filecheck-may-segfault %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Testany@.*: Argument 2 [(]array_of_requests[)] has to be an array of predefined or user defined requests, the following entries are unknown requests}}

/**
 *  @file
 *
 *  Simple test program.
 *  It just calls
 *  - MPI_Init
 *  - MPI_Finalize
 *  - MPI_Comm_rank
 *  - MPI_Comm_size
 *  - MPI_Isend
 *  - MPI_Issend
 *  - MPI_Testany
 *  - MPI_Recv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: testany-nvr2.c 997 2009-10-02 14:44:28Z hpczink $
 */

/*
  ** We have 3 requests in the array - one is reused (but freed before),
  ** one usual and one which is non-valid, but no matching recv for the
  ** valid request
  */

#include <stdio.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <unistd.h>
#endif
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
    int index = -1;
    int flag = 0;

    MPI_Status status;
    MPI_Status status_arr[3];

    MPI_Request request_arr[3];

    printf("We call Testany with 3 requests: REQUEST_NULL, valid, non-valid.\n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        /* going to receive message */
        MPI_Recv(&value2, COUNT, MPI_INT, 1, MSG_TAG_1, MPI_COMM_WORLD, &status);
        assert(value2 == 19);
    }

    if (rank == 1) {
        /* going to send message */
        value = 19;
        MPI_Isend(&value, COUNT, MPI_INT, 0, MSG_TAG_1, MPI_COMM_WORLD, &request_arr[0]);
        MPI_Wait(&request_arr[0], &status);
        MPI_Issend(&value, COUNT, MPI_INT, 0, MSG_TAG_2, MPI_COMM_WORLD, &request_arr[1]);
#ifdef _WIN32
        Sleep(1000);
#else
        sleep(1);
#endif
        MPI_Testany(3, request_arr, &index, &flag, status_arr);
        if (0 != flag) {
            printf(" index(of now freed request)= %d \n", index);
        }
    }

    MPI_Finalize();

    return 1;
}
