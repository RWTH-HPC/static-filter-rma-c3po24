// RUN: %must-run %mpiexec-numproc-flag 6 \
// RUN: %must-bin-dir/marmot_c_comm-compare 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are 1 communicators that are not freed when MPI_Finalize was issued}}

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
 *  - MPI_Comm_compare
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: comm-compare.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int dims[2] = {2, 3};
    int periods[2] = {0, 0};
    int result = 0;

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

        MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, 1, &comm_cart);
        MPI_Comm_compare(MPI_COMM_WORLD, comm_cart, &result);
        if (rank == 0) {
            switch (result) {
            case (MPI_UNEQUAL):
                printf("comms are unequal \n");
                break;
            case (MPI_IDENT):
                printf("comms are identical \n");
                break;
            case (MPI_CONGRUENT):
                printf("comms are congruent \n");
                break;
            case (MPI_SIMILAR):
                printf("comms are similar \n");
                break;
            default:
                /* do nothing. */
                break;
            } /* end switch */
        }     /* end if */
    }         /* end else */

    MPI_Finalize();

    return 1;
}
