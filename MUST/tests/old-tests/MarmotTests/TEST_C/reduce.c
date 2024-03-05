// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_reduce 2>&1 \
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
 *  - MPI_Reduce
 *  - MPI_Barrier
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: reduce.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    const int COUNT = 1;

    int size = -1;
    int rank = -1;
    int sendvalue = -1;
    int recvvalue = -1;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    sendvalue = size - rank;

    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Reduce(&sendvalue, &recvvalue, COUNT, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);

    if (rank == 0) {
        assert(recvvalue == size);
    }

    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Reduce(&sendvalue, &recvvalue, COUNT, MPI_INT, MPI_MIN, 0, MPI_COMM_WORLD);

    if (rank == 0) {
        assert(recvvalue == 1);
    }

    MPI_Finalize();

    return 0;
}
