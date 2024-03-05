// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_deadlock2 \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Isend@.*: The memory regions to be transfered by this send operation overlap with regions spanned by a pending non-blocking receive operation!}}

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
 *  - MPI_Send
 *  - MPI_Recv
 *
 *  This program produces a deadlock.
 *  At leat 2 nodes are required to run the program.
 *
 *  Rank 0 recv a message from Rank 1.
 *  Rank 1 recv a message from Rank 0.
 *
 *  AFTERWARDS:
 *  Rank 0 sends a message to Rank 1.
 *  Rank 1 sends a message to Rank 0.
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: deadlock2.c 401 2005-09-08 11:57:12Z rusbetti $  
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    const int COUNT = 1;
    const int MSG_TAG_1 = 17;
    const int MSG_TAG_2 = 18;

    int rank = -1;
    int size = -1;
    int dummy = 0;

    MPI_Request r1, r2, r3, r4;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (size < 2) {
        fprintf(stderr, " This program needs at least 2 PEs!\n");
    } else {
        if (rank == 0) {
            MPI_Irecv(&dummy, COUNT, MPI_INT, 1, MSG_TAG_1, MPI_COMM_WORLD, &r3);
            MPI_Isend(&dummy, COUNT, MPI_INT, 1, MSG_TAG_2, MPI_COMM_WORLD, &r1);
        }
        if (rank == 1) {
            MPI_Isend(&dummy, COUNT, MPI_INT, 0, MSG_TAG_1, MPI_COMM_WORLD, &r2);
            MPI_Irecv(&dummy, COUNT, MPI_INT, 0, MSG_TAG_2, MPI_COMM_WORLD, &r4);
        }
    }

    MPI_Finalize();

    return 0;
}
