// RUN: %must-run %mpiexec-numproc-flag 5 \
// RUN: %must-bin-dir/marmot_c_reduce_scatter 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Reduce_scatter@.*: Argument . [(]recvcounts[)] is an array that contains zero value[(]s[)], which is correct but unusual. [(]recvcounts.*=0[)]}}

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
 *  - MPI_Reduce-scatter
 *  - MPI_Barrier
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: reduce_scatter.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int i = 0;
    int size = -1;
    int rank = -1;
    int sendvalue[5] = {0, 0, 0, 0, 0};
    int recvvalue[2] = {0, 0};
    int* recvcounts = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (size < 5) {
        printf("We need at least 5 nodes! \n");
    } else {
        if (rank < 4) {
            for (i = 0; i < 5; i++)
                sendvalue[i] = size + rank - i;
        } else {
            for (i = 0; i < 5; i++) {
                sendvalue[i] = 0;
            }
        }

        /* define recvcounts */
        recvcounts = (void*)malloc(size * sizeof(int));
        for (i = 0; i < 4; i++) {
            recvcounts[i] = 1;
        }
        recvcounts[2] = 2;
        for (i = 4; i < size; i++) {
            recvcounts[i] = 0;
        }

        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Reduce_scatter(&sendvalue, &recvvalue, recvcounts, MPI_INT, MPI_MAX, MPI_COMM_WORLD);

        switch (rank) {
        case (0):
            assert(recvvalue[0] == size + 3);
            break;
        case (1):
            assert(recvvalue[0] == size + 2);
            break;
        case (2):
            assert(recvvalue[0] == size + 1);
            assert(recvvalue[1] == size);
            break;
        case (3):
            assert(recvvalue[0] == size - 1);
            break;
        default:
            /* do nothing. */
            break;
        }

        free(recvcounts);
    } /* end of else (enough nodes) */

    MPI_Finalize();

    return 0;
}
