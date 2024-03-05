// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_allreduce \
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
 *  - MPI_Allreduce
 *  - MPI_Barrier
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: allreduce.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
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
    MPI_Allreduce(&sendvalue, &recvvalue, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
    assert(recvvalue = size);

    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Allreduce(&sendvalue, &recvvalue, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);
    assert(recvvalue == 1);

    MPI_Finalize();

    return 0;
}
