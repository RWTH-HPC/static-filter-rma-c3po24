// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_get-count \
// RUN: 2>&1 \
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
 *  - MPI_Send
 *  - MPI_Recv
 *  - MPI_Probe
 *  - MPI_Get_count
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: get-count.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    const int MSG_TAG = 17;

    int size = -1;
    int rank = -1;
    int value = -1;
    int count = -1;

    MPI_Status status;
    MPI_Status status2;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        MPI_Probe(1, MSG_TAG, MPI_COMM_WORLD, &status2);
        printf("Message can be received \n");
        MPI_Get_count(&status2, MPI_INT, &count);
        /* going to receive message */
        MPI_Recv(&value, count, MPI_INT, 1, MSG_TAG, MPI_COMM_WORLD, &status);
        assert(value == 19);
    }

    if (rank == 1) {
        /* going to send message */
        value = 19;
        count = 1;
        MPI_Send(&value, count, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD);
    }

    MPI_Finalize();

    return 0;
}
