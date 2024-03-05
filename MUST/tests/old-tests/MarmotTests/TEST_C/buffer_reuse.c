// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_buffer_reuse 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

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
 * This programm is erroneous as both Isends use the same buffer !
 *
 *  @author Tobias Hilbrich
 *
 *  $Id$  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    const int COUNT = 1;
    const int MSG_TAG_1 = 1;
    const int MSG_TAG_2 = 2;

    int size = -1;
    int rank = -1;
    int value = -1;
    int value2 = -1;

    MPI_Status status;
    MPI_Request r1, r2;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (size < 2) {
        printf("Sry not enough PE's !");
        return 0;
    }

    if (rank == 0) {
        /* going to receive message */
        MPI_Recv(&value, COUNT, MPI_INT, 1, MSG_TAG_1, MPI_COMM_WORLD, &status);
        MPI_Recv(&value, COUNT, MPI_INT, 1, MSG_TAG_2, MPI_COMM_WORLD, &status);
    }

    if (rank == 1) {
        /* going to send message */
        value2 = 19;
        MPI_Isend(&value2, COUNT, MPI_INT, 0, MSG_TAG_1, MPI_COMM_WORLD, &r1);
        MPI_Isend(&value2, COUNT, MPI_INT, 0, MSG_TAG_2, MPI_COMM_WORLD, &r2);

        MPI_Wait(&r1, &status);
        MPI_Wait(&r2, &status);
    }

    MPI_Finalize();

    return 0;
}
