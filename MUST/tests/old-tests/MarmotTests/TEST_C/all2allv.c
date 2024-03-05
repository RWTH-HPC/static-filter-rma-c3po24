// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_all2allv \
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
 *  - MPI_Barrier
 *  - MPI_Alltoallv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: all2allv.c 319 2004-08-16 11:25:02Z rusbetti $  
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
    int* recvcounts = 0;
    int* sendcounts = 0;
    int* sdispls = 0;
    int* rdispls = 0;

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

        sendcounts = (int*)malloc(size * sizeof(int));
        sdispls = (int*)malloc(size * sizeof(int));

        if (rank < 4) {
            for (i = 0; i < size; i++) {
                sendcounts[i] = rank;
                sdispls[i] = 0;
            }
        }

        if (rank > 3) {
            for (i = 0; i < size; i++) {
                sendcounts[i] = 0;
            }
        }

        recvcounts = (int*)malloc(size * sizeof(int));
        rdispls = (int*)malloc(size * sizeof(int));
        used = 0;

        for (i = 0; i < 4; i++) {
            recvcounts[i] = i;
            rdispls[i] = used;
            used += i;
        }

        for (i = 4; i < size; i++) {
            recvcounts[i] = 0;
            rdispls[i] = used + 1;
        }

        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Alltoallv(
            sendbuf,
            sendcounts,
            sdispls,
            MPI_INT,
            recvbuf,
            recvcounts,
            rdispls,
            MPI_INT,
            MPI_COMM_WORLD);

        /*         assert(recvbuf[0] == 0); */
        /*         assert(recvbuf[1] == 1); */
        /*         assert(recvbuf[2] == 2); */
        /*         assert(recvbuf[3] == 1); */
        /*         assert(recvbuf[4] == 2); */
        /*         assert(recvbuf[5] == 3); */

        free(recvcounts);
        free(sendcounts);
        free(sdispls);
        free(rdispls);
    }

    MPI_Finalize();

    return 0;
}
