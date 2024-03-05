// RUN: %must-run %mpiexec-numproc-flag 4 %must-bin-dir/marmot_c_scatterv \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Scatterv@.*: Argument 6 [(]recvcount[)] is zero, which is correct but unusual}}

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
 *  - MPI_Scatterv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: scatterv.c 319 2004-08-16 11:25:02Z rusbetti $  
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
    int recvbuf[3] = {0, 0, 0};
    int sendbuf[6] = {1, 1, 2, 1, 2, 3};
    int used = 0;
    int* sendcounts = 0;
    int recvcount = -1;
    int* displs = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (size < 4) {
        printf(" We need at least 4 nodes! \n");
    } else {
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        printf(" I am rank %d of %d PEs\n", rank, size);

        recvcount = rank;
        if (rank > 3) {
            recvcount = 0;
        }

        MPI_Barrier(MPI_COMM_WORLD);

        if (rank == 0) {
            sendcounts = (int*)malloc(size * sizeof(int));
            displs = (int*)malloc(size * sizeof(int));
            for (i = 0; i <= 3; i++) {
                sendcounts[i] = i;
                displs[i] = used;
                /* displs[i]=i; */
                used += i;
            }
            if (size > 4) {
                for (i = 4; i < size; i++) {
                    sendcounts[i] = 0;
                }
            }
        }

        MPI_Scatterv(
            sendbuf,
            sendcounts,
            displs,
            MPI_INT,
            recvbuf,
            recvcount,
            MPI_INT,
            0,
            MPI_COMM_WORLD);

        if (rank == 1) {
            assert(*recvbuf == 1);
        }

        if (rank == 2) {
            assert(*recvbuf == 1);
            assert(*(sendbuf + sizeof(int)) == 2);
        }

        if (rank == 3) {
            assert(*(sendbuf) == 1);
            assert(*(sendbuf + sizeof(int)) == 2);
            /*            assert(*(sendbuf + 2 * sizeof(int)) == 3); */
        }

        if (rank == 0) {
            free(sendcounts);
            free(displs);
        }
    }

    MPI_Finalize();

    return 0;
}
