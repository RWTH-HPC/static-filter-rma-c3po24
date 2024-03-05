// RUN: %must-run %mpiexec-numproc-flag 3 %must-bin-dir/marmot_c_basic_mult2 \
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
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: basic_mult2.c 997 2009-10-02 14:44:28Z hpczink $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    const int MSG_TAG = 17;
    const int COUNT = 1;
    /* const int ARRAY_LENGTH = 10; */
    enum { ARRAY_LENGTH = 10 };

    int i;
    int size = -1;
    int rank = -1;
    int value0 = -1;
    int value = -1;
    int value2 = -1;
    int arr0[ARRAY_LENGTH];
    int arr1[ARRAY_LENGTH];
    int arr2[ARRAY_LENGTH];

    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (size < 3) {
        printf("At least 3 nodes for application needed ! \n");
    } else {
        for (i = 0; i < ARRAY_LENGTH; i++) {
            arr0[i] = -1;
            arr1[i] = -1;
            arr2[i] = -1;
        }

        if (rank == 0) {
            MPI_Recv(&value0, COUNT, MPI_INT, 1, MSG_TAG, MPI_COMM_WORLD, &status);
            printf("value0: %d \n", value0);
            assert(value0 == 19);

            MPI_Send(&value0, COUNT, MPI_INT, 2, MSG_TAG, MPI_COMM_WORLD);
        }

        if (rank == 1) {
            value = 19;

            MPI_Send(&value, COUNT, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD);
            printf("value: %d \n", value);

            MPI_Recv(&value, COUNT, MPI_INT, 2, MSG_TAG, MPI_COMM_WORLD, &status);
            printf("value: %d \n", value);
            /*            assert(value == 19); */
        }

        if (rank == 2) {
            MPI_Send(&value2, COUNT, MPI_INT, 1, MSG_TAG, MPI_COMM_WORLD);
            printf("value2: %d \n", value2);

            MPI_Recv(&value2, COUNT, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD, &status);
            printf("value2: %d \n", value2);

            assert(value2 == 19);
        }

        MPI_Barrier(MPI_COMM_WORLD);

        if (rank == 0) {
            for (i = 0; i < ARRAY_LENGTH; i++)
                arr0[i] = 37;
            MPI_Bcast(&arr0, ARRAY_LENGTH, MPI_INT, 0, MPI_COMM_WORLD);
        }

        if (rank == 1) {
            MPI_Bcast(&arr1, ARRAY_LENGTH, MPI_INT, 0, MPI_COMM_WORLD);
            assert(arr1[0] == 37);
            assert(arr1[9] == 37);
        }

        if (rank >= 2) {
            MPI_Bcast(&arr2, ARRAY_LENGTH, MPI_INT, 0, MPI_COMM_WORLD);
            assert(arr2[0] == 37);
            assert(arr2[9] == 37);
        }
    } /* end else */

    MPI_Finalize();

    return 0;
}
