// RUN: %must-run %mpiexec-numproc-flag 6 \
// RUN: %must-bin-dir/marmot_c_comm-create2 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Comm_create@.*: Argument 1 [(]comm[)] is a communicator that already had a process topology.}}

/**
 *  @file
 *
 *  Simple test program.
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Finalize 
 *  - MPI_Comm_rank
 *  - MPI_Comm_size
 *  - MPI_Cart_create
 *  - MPI_Comm_create
 *  - MPI_Comm_group
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: comm-create2.c 319 2004-08-16 11:25:02Z rusbetti $  
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

    MPI_Comm comm_cart;
    MPI_Comm newcomm;

    MPI_Group worldgroup;

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
        MPI_Comm_group(MPI_COMM_WORLD, &worldgroup);
        MPI_Comm_create(comm_cart, worldgroup, &newcomm);
        /* MPI_Comm_create(MPI_COMM_WORLD, worldgroup, &newcomm); */
    }

    MPI_Finalize();

    return 1;
}
