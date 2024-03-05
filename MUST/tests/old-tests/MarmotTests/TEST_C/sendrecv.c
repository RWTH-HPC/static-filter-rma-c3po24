// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_sendrecv \
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
 *  - MPI_Sendrecv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: sendrecv.c 394 2005-09-06 13:30:31Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    const int COUNT = 1;
    const int MSG_TAG = 17;

    int size = -1;
    int rank = -1;
    int buf1 = -1;
    int buf2 = -1;
    int buf3 = -1;
    int dest = -1;
    int src = -1;

    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    dest = (rank + 1) % size;
    src = (rank - 1 + size) % size;
    buf1 = 37;
    buf3 = 42;

    MPI_Sendrecv(
        &buf1,
        COUNT,
        MPI_INT,
        dest,
        MSG_TAG,
        &buf2,
        COUNT,
        MPI_INT,
        src,
        MSG_TAG,
        MPI_COMM_WORLD,
        &status);
    //        MPI_Sendrecv(&buf3, COUNT, MPI_INT, src, MSG_TAG, &buf4, COUNT, MPI_INT,
    //                     dest, MSG_TAG, MPI_COMM_WORLD, &status);

    assert(buf2 == 37);
    //    assert(buf4 == 42);

    MPI_Finalize();

    return 0;
}
