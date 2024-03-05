// XFAIL: *
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_cancel 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*P2PMatch: detected a cancel, not supported, outputs may be wrong!}}

/**
 *  @file
 *
 *  Simple test program.
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Finalize 
 *  - MPI_Comm_rank
 *  - MPI_Comm_size
 *  - MPI_Send_init
 *  - MPI_Start
 *  - MPI_Cancel
 *  - MPI_Test_cancel
 *  - MPI_Wait
 *  - MPI_Request_free
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: cancel.c 319 2004-08-16 11:25:02Z rusbetti $  
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
    int flag = 0;

    MPI_Status status;
    MPI_Request request;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 1) {
        /* going to send message */
        MPI_Send_init(&value, COUNT, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD, &request);
        MPI_Start(&request);
        MPI_Cancel(&request);
        MPI_Wait(&request, &status);
        MPI_Test_cancelled(&status, &flag);
        if (0 != flag) {
            printf(" msg cancelled \n");
        }
        MPI_Request_free(&request);
    }

    MPI_Finalize();

    return 0;
}
