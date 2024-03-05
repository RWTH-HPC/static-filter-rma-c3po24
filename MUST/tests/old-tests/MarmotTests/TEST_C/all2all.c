// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_all2all \
// RUN: 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

/**
 *  @file
 *
 *  Simple MPI program for testing MARMOT.
 *
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Finalize 
 *  - MPI_Comm_rank
 *  - MPI_Comm_size
 *  - MPI_Barrier
 *  - MPI_Alltoall
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: all2all.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int* sendbuf = 0;
    int* recvbuf = 0;
    int i = 0;
    const int COUNT = 1;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    recvbuf = (int*)malloc(size * sizeof(int));
    sendbuf = (int*)malloc(size * sizeof(int));

    for (i = 0; i < size; i++) {
        sendbuf[i] = i;
    }
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Alltoall(sendbuf, COUNT, MPI_INT, recvbuf, COUNT, MPI_INT, MPI_COMM_WORLD);

    for (i = 0; i < size; i++) {
        assert(recvbuf[i] == rank);
    }

    free(recvbuf);
    free(sendbuf);

    MPI_Finalize();

    return 0;
}
