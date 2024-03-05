// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_request-reuse1 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-RUNTIME] ERROR: MUST detected a deadlock, detailed information is available in the MUST output file

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
 *  $Id: request-reuse1.c 1012 2009-10-20 19:34:56Z tobias $
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
    const int MSG_TAG_1 = 16;
    const int MSG_TAG_2 = 17;
    const int MSG_TAG_3 = 18;

    int size = -1;
    int rank = -1;
    int value = -1;
    int value2 = -1;

    MPI_Status send_status, recv_status;

    MPI_Request send_request, recv_request;

    printf("We call Irecv and Isend with non-freed requests.\n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        /* This is just to get the request used */
        MPI_Isend(&value, COUNT, MPI_INT, 1, MSG_TAG_1, MPI_COMM_WORLD, &recv_request);

        /* going to receive the message and reuse a non-freed request */
        MPI_Irecv(&value, COUNT, MPI_INT, 1, MSG_TAG_2, MPI_COMM_WORLD, &recv_request);
        MPI_Wait(&recv_request, &recv_status);
        assert(value == 19);
    }

    if (rank == 1) {
        value2 = 19;
        /* this is just to use the request */
        MPI_Isend(&value, COUNT, MPI_INT, 0, MSG_TAG_3, MPI_COMM_WORLD, &send_request);

        /* going to send the message */
        MPI_Isend(&value2, COUNT, MPI_INT, 1, MSG_TAG_2, MPI_COMM_WORLD, &send_request);
        MPI_Wait(&send_request, &send_status);
    }

    MPI_Finalize();

    return 0;
}
