// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_pers_requests_nor 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Startall@.*: Argument 1 [(]count[)] is zero, which is correct but unusual!}}

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
 *  - MPI_Send_init
 *  - MPI_Startall
 *  - MPI_Waitall
 *  - MPI_Recv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: pers_requests_nor.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

/*
  ** We have 3 requests in the array - one is reused (but freed before),
  ** one usual and one which is non-valid
  */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    const int COUNT = 1;
    const int MSG_TAG_1 = 17;
    const int MSG_TAG_2 = 18;
    const int MSG_TAG_3 = 19;

    int size = -1;
    int rank = -1;
    int value[3] = {0, 0, 0};
    int value2[3] = {0, 0, 0};
    int i = 0;

    MPI_Request request_arr[3];

    printf("We do Startall with no requests. \n");

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    for (i = 0; i <= 2; i++) {
        value[i] = 37;
        value2[i] = -1;
    }

    if (rank == 0) {
        /* going to receive message */
        /*  MPI_Recv(&value2[0],COUNT,MPI_INT,1,MSG_TAG_1,MPI_COMM_WORLD,&status[0]); */
        /*  MPI_Recv(&value2[1],COUNT,MPI_INT,1,MSG_TAG_2,MPI_COMM_WORLD,&status[1]); */
        /*  MPI_Recv(&value2[2],COUNT,MPI_INT,1,MSG_TAG_3,MPI_COMM_WORLD,&status[2]); */
    }

    if (rank == 1) {
        /* going to send message */
        MPI_Send_init(&value[0], COUNT, MPI_INT, 0, MSG_TAG_1, MPI_COMM_WORLD, &request_arr[0]);
        MPI_Send_init(&value[0], COUNT, MPI_INT, 0, MSG_TAG_2, MPI_COMM_WORLD, &request_arr[1]);
        MPI_Send_init(&value[0], COUNT, MPI_INT, 0, MSG_TAG_3, MPI_COMM_WORLD, &request_arr[2]);
        MPI_Startall(0, request_arr);
    }

    MPI_Finalize();

    return 0;
}
