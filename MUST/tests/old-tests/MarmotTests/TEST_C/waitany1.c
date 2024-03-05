// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_waitany1 \
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
 *  - MPI_Isend
 *  - MPI_Waitany
 *  - MPI_Waitall
 *  - MPI_Recv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: waitany1.c 997 2009-10-02 14:44:28Z hpczink $
 */

#include <stdio.h>
#include <assert.h>
/* #include <time.h>
 * use instead
 */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <unistd.h>
#endif
#include "mpi.h"

int main(int argc, char** argv)
{
    const int COUNT = 1;
    const int MSG_TAG_1 = 17;
    const int MSG_TAG_2 = 18;
    const int MSG_TAG_3 = 19;
    const int MSG_TAG_4 = 20;
    const int MSG_TAG_5 = 21;

    int size = -1;
    int rank = -1;
    int value[5] = {0, 0, 0, 0, 0};
    int value2[5] = {0, 0, 0, 0, 0};
    int i = 0;
    int index[1] = {-1};

    MPI_Status status[5];
    MPI_Status status_arr[5];
    MPI_Status status1;

    MPI_Request request_arr[5];

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    for (i = 0; i <= 4; i++) {
        value[i] = 37;
        value2[i] = -1;
    }

    if (rank == 0) {
        /* going to receive message */
#ifdef _WIN32
        Sleep(2000);
#else
        sleep(2);
#endif
        MPI_Recv(&value2[0], COUNT, MPI_INT, 1, MSG_TAG_2, MPI_COMM_WORLD, &status[0]);
        MPI_Recv(&value2[1], COUNT, MPI_INT, 1, MSG_TAG_1, MPI_COMM_WORLD, &status[1]);
#ifdef _WIN32
        Sleep(1000);
#else
        sleep(1);
#endif
        MPI_Recv(&value2[2], COUNT, MPI_INT, 1, MSG_TAG_3, MPI_COMM_WORLD, &status[2]);
        MPI_Recv(&value2[3], COUNT, MPI_INT, 1, MSG_TAG_4, MPI_COMM_WORLD, &status[3]);
        MPI_Recv(&value2[4], COUNT, MPI_INT, 1, MSG_TAG_5, MPI_COMM_WORLD, &status[4]);
    }

    if (rank == 1) {
        /* going to send message */
        MPI_Issend(&value[0], COUNT, MPI_INT, 0, MSG_TAG_1, MPI_COMM_WORLD, &request_arr[0]);
        MPI_Isend(&value[1], COUNT, MPI_INT, 0, MSG_TAG_2, MPI_COMM_WORLD, &request_arr[1]);
        MPI_Issend(&value[2], COUNT, MPI_INT, 0, MSG_TAG_3, MPI_COMM_WORLD, &request_arr[2]);
        MPI_Issend(&value[3], COUNT, MPI_INT, 0, MSG_TAG_4, MPI_COMM_WORLD, &request_arr[3]);
        MPI_Isend(&value[4], COUNT, MPI_INT, 0, MSG_TAG_5, MPI_COMM_WORLD, &request_arr[4]);
        printf("going to do Waitany \n");
        MPI_Waitany(5, request_arr, &index[0], &status1);
        printf("Waitany: index= %d \n", *index);
        MPI_Waitall(5, request_arr, status_arr);
    }

    MPI_Finalize();

    return 0;
}
