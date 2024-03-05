// RUN: %must-run %mpiexec-numproc-flag 6 %must-bin-dir/marmot_c_cart-shift \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are 1 communicators that are not freed when MPI_Finalize was issued, a quality application should free all MPI resources before calling MPI_Finalize}}

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
 *  $Id: cart-shift.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int dims[2] = {2, 3};
    int periods[2] = {0, 1};
    int rank_source = 0;
    int rank_dest = 0;

    MPI_Comm comm_cart;

    printf("dim1=%d, dim2=%d \n", dims[0], dims[1]);
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (size != (dims[0] * dims[1])) {
        if (rank == 0) {
            printf("Number of nodes not matching new topology! \n");
        }
    } else {
        printf(" I am rank %d of %d PEs\n", rank, size);

        MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, 0, &comm_cart);
        MPI_Cart_shift(comm_cart, 0, 1, &rank_source, &rank_dest);
        /*if(rank_source==MPI_PROC_NULL)
           printf("node%d: no lower neighbour (in direction 0) \n",rank);
           if(rank_dest==MPI_PROC_NULL)
           printf("node%d: no upper neighbour (in direction 0)\n",rank); 
         */
        MPI_Cart_shift(comm_cart, 1, 1, &rank_source, &rank_dest);
        /*
           if(rank_source==MPI_PROC_NULL)
           printf("node%d: no lower neighbour (in direction 1) \n",rank);
           if(rank_dest==MPI_PROC_NULL)
           printf("node%d: no upper neighbour (in direction 1)\n",rank);
         */
    }

    MPI_Finalize();

    return 0;
}
