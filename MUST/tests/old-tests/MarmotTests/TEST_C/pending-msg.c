// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_pending-msg \
// RUN: 2>&1 \
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
 *  - MPI_Recv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: pending-msg.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    const int COUNT = 1;
    const int MSG_TAG_1 = 17;
    const int MSG_TAG_2 = 19;

    int size = -1;
    int rank = -1;
    int value = -1;
    int value2 = -1;

    MPI_Status status;

    printf("We call Finalize when there is still a non-received \
message pending \n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        /* going to receive message */
        MPI_Recv(&value, COUNT, MPI_INT, 1, MSG_TAG_2, MPI_COMM_WORLD, &status);
        assert(value == 19);
    }

    if (rank == 1) {
        /* going to send message */
        value2 = 19;
        MPI_Send(&value2, COUNT, MPI_INT, 0, MSG_TAG_1, MPI_COMM_WORLD);
        MPI_Send(&value2, COUNT, MPI_INT, 0, MSG_TAG_2, MPI_COMM_WORLD);
    }

    MPI_Finalize();

    return 0;
}
