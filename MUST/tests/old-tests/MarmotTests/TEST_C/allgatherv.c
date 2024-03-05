// RUN: %must-run %mpiexec-numproc-flag 4 %must-bin-dir/marmot_c_allgatherv \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Allgatherv@.*: Argument 5 [(]recvcounts[)] is an array that contains zero value[(]s[)], which is correct but unusual}}

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
 *  - MPI_Barrier
 *  - MPI_Allgatherv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: allgatherv.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int i;
    int size = -1;
    int rank = -1;
    int sendbuf[3] = {0, 0, 0};
    int recvbuf[6] = {0, 0, 0, 0, 0, 0};
    int used = 0;
    int* recvcount = 0;
    int sendcount = -1;
    int* displs = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 4) {
        printf(" We need at least 4 nodes! \n");
    } else {
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        printf(" I am rank %d of %d PEs\n", rank, size);

        for (i = 0; i <= 2; i++) {
            sendbuf[i] = i;
        }

        sendcount = rank;
        if (rank > 3) {
            sendcount = 0;
        }

        recvcount = (int*)malloc(size * sizeof(int));
        displs = (int*)malloc(size * sizeof(int));

        for (i = 0; i <= 3; i++) {
            recvcount[i] = i;
            displs[i] = used;
            /* displs[i]=i; */
            used += i;
        }

        if (size > 4) {
            for (i = 4; i < size; i++) {
                recvcount[i] = 0;
            }
        }

        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Allgatherv(
            &sendbuf,
            sendcount,
            MPI_INT,
            recvbuf,
            recvcount,
            displs,
            MPI_INT,
            MPI_COMM_WORLD);

        /*         assert(recvbuf[0] == 1); */
        /*         assert(recvbuf[1] == 1); */
        /*         assert(recvbuf[2] == 2); */
        /*         assert(recvbuf[3] == 1); */
        /*         assert(recvbuf[4] == 2); */
        /*         assert(recvbuf[5] == 3); */

        free(recvcount);
        free(displs);
    } /* end else */

    MPI_Finalize();

    return 0;
}
