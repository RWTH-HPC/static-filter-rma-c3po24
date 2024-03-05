// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_test_1 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

/**
 *  @file
 *
 *  Simple test program.
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Finalize 
 *  - MPI_Comm_rank
 *  - MPI_Comm_size
 *  - MPI_Isend
 *  - MPI_Wait
 *  - MPI_Test
 *  - MPI_Irecv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: test_1.c 319 2004-08-16 11:25:02Z rusbetti $  
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
    int flag = 0;

    MPI_Status send_status;
    MPI_Status recv_status;

    MPI_Request send_request;
    MPI_Request recv_request;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    fprintf(stderr, " I am rank %d of %d PEs\n", rank, size);
    fflush(stderr);

    if (rank == 0) {
        /* going to receive message */
        MPI_Irecv(&value, COUNT, MPI_INT, 1, MSG_TAG, MPI_COMM_WORLD, &recv_request);
        MPI_Test(&recv_request, &flag, &recv_status);
        if (0 == flag) {
            printf("flag==false, going to do Wait. \n");
            MPI_Wait(&recv_request, &recv_status);
        }
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
