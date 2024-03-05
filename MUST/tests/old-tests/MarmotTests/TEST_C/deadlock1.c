// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_deadlock1 \
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
 *  $Id: deadlock1.c 394 2005-09-06 13:30:31Z rusbetti $  
 */

#include <stdio.h>
#include "mpi.h"

#ifdef MARMOT_VAMPI
#include "replacempi.h"
#include "marmot.h"
#endif

int main(int argc, char** argv)
{
    const int COUNT = 1;
    const int MSG_TAG_1 = 17;
    const int MSG_TAG_2 = 18;

    int rank = -1;
    int size = -1;
    int dummy = 0;

    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (size < 2) {
        fprintf(stderr, " This program needs at least 2 PEs!\n");
    } else {
        if (rank == 0) {
            MPI_Recv(&dummy, COUNT, MPI_INT, 1, MSG_TAG_1, MPI_COMM_WORLD, &status);
            MPI_Send(&dummy, COUNT, MPI_INT, 1, MSG_TAG_2, MPI_COMM_WORLD);
        }
        if (rank == 1) {
            MPI_Recv(&dummy, COUNT, MPI_INT, 0, MSG_TAG_2, MPI_COMM_WORLD, &status);
            MPI_Send(&dummy, COUNT, MPI_INT, 0, MSG_TAG_1, MPI_COMM_WORLD);
        }
    }

    MPI_Finalize();

    return 0;
}
