// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_gather 2>&1 \
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
 *  - MPI_Gather
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: gather.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include <mpi.h>

int main(int argc, char** argv)
{
    const int COUNT = 1;

    int size = -1;
    int rank = -1;
    int sendbuf = -1;
    int* recvbuf = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    recvbuf = (void*)malloc(size * sizeof(MPI_INT));
    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0) {
        MPI_Gather(&sendbuf, COUNT, MPI_INT, recvbuf, COUNT, MPI_INT, 0, MPI_COMM_WORLD);
    }

    /* why assert(..)? */
    /*assert(*recvbuf==37); */

    if (rank != 0) {
        sendbuf = 37;
        MPI_Gather(&sendbuf, COUNT, MPI_INT, recvbuf, COUNT, MPI_INT, 0, MPI_COMM_WORLD);
    }

    free(recvbuf);

    MPI_Finalize();

    return 0;
}
