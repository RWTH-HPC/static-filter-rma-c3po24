// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_cart-rank-nvcomm 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Cart_rank@.*: Argument 1 [(]comm[)] is a communicator with no cartesian topology and was used where such a topology is required}}

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
 *  - MPI_Cart_create
 *  - MPI_Cart_rank
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: cart-rank-nvcomm.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int coords[2] = {0, 0};
    int out_rank = -1;

    printf("Call of MPI_Cart_rank with non cartesian communicator \n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    printf(" I am rank %d of %d PEs\n", rank, size);

    MPI_Cart_rank(MPI_COMM_WORLD, coords, &out_rank);

    MPI_Finalize();

    return 0;
}
