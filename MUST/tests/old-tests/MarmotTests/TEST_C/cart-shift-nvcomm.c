// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_cart-shift-nvcomm 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Cart_shift@.*: Argument 1 [(]comm[)] is a communicator with no cartesian topology and was used where such a topology is required}}

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
 *  - MPI_Cart_shift
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: cart-shift-nvcomm.c 309 2004-08-11 11:12:56Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int rank_source = 0;
    int rank_dest = 0;

    printf("Call of MPI_Carts_shift with non cartesian communicator \n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    printf(" I am rank %d of %d PEs\n", rank, size);

    MPI_Cart_shift(MPI_COMM_WORLD, 0, 1, &rank_source, &rank_dest);
    /*if(rank_source==MPI_PROC_NULL)
       printf("node%d: no lower neighbour (in direction 0) \n",rank);
       if(rank_dest==MPI_PROC_NULL)
       printf("node%d: no upper neighbour (in direction 0)\n",rank); 
     */

    MPI_Finalize();

    return 0;
}
