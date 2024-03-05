// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_bsend 2>&1 \
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
 *  - MPI_Bsend
 *  - MPI_Recv
 *  - MPI_Buffer_attach
 *  - MPI_Buffer_detach
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: bsend.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    const int COUNT = 1;
    const int MSG_TAG = 17;

    int size = -1;
    int rank = -1;
    int buff_size = -1;
    int value = -1;
    int value2 = -1;

    signed char* value3 = 0;

    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        /* going to receive the message */
        MPI_Recv(&value, COUNT, MPI_INT, 1, MSG_TAG, MPI_COMM_WORLD, &status);
    }

    if (rank == 1) {
        value2 = 19;

        buff_size = sizeof(MPI_INT) + MPI_BSEND_OVERHEAD;
        value3 = (signed char*)malloc(buff_size);
        MPI_Buffer_attach(value3, buff_size);
        /* going to send the message */
        MPI_Bsend(&value3, COUNT, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD);
        MPI_Buffer_detach(value3, &buff_size);
        free(value3);
    }

    MPI_Finalize();

    return 0;
}
