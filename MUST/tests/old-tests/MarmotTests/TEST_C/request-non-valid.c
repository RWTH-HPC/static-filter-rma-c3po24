// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_request-non-valid 2>&1 \
// RUN: | %filecheck-may-segfault %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Wait@.*: Argument 1 [(]request[)] is a unknown request}}

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
 *  - MPI_Irecv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: request-non-valid.c 319 2004-08-16 11:25:02Z rusbetti $  
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
    MPI_Status recv_status;

    MPI_Request send_request;
    MPI_Request recv_request;
    MPI_Request request;

    printf("We call Wait with non-valid request.\n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        /* going to receive message */
        MPI_Irecv(&value, COUNT, MPI_INT, 1, MSG_TAG, MPI_COMM_WORLD, &recv_request);
        MPI_Wait(&recv_request, &recv_status);
        /* use a nonexisting request */
        MPI_Wait(&request, &recv_status);
        assert(value == 19);
    }

    if (rank == 1) {
        /* going to send message */
        value2 = 19;
        MPI_Isend(&value2, COUNT, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD, &send_request);
        MPI_Wait(&send_request, &send_status);
    }

    MPI_Finalize();

    return 0;
}
