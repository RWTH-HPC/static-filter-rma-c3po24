// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 6 \
// RUN: %must-bin-dir/marmot_c_group-translate-ranks2 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Comm_group@.*: Argument 1 [(]comm[)] is MPI_COMM_NULL where a valid communicator was expected}}

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
 *  - MPI_Comm_group
 *  - MPI_Comm_free
 *  - MPI_Group_translate_ranks
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: group-translate-ranks2.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int notfound = 0;
    int i = 0;
    int rank = -1;
    int dims[2] = {2, 3};
    int periods[2] = {0, 0};
    int* ranks1 = 0;
    int* ranks2 = 0;

    MPI_Comm comm_cart;

    MPI_Group worldgroup;
    MPI_Group comm_group;

    printf("Here we use a comm/group we already freed. \n");
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

        ranks1 = (int*)malloc(sizeof(MPI_INT) * size);
        ranks2 = (int*)malloc(sizeof(MPI_INT) * size);

        MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, 1, &comm_cart);
        MPI_Comm_group(MPI_COMM_WORLD, &worldgroup);
        MPI_Comm_free(&comm_cart);
        MPI_Comm_group(comm_cart, &comm_group);
        MPI_Group_free(&comm_group);
        MPI_Group_translate_ranks(worldgroup, size, ranks1, comm_group, ranks2);
        for (i = 0; i <= size; i++) {
            if (ranks2[i] == MPI_UNDEFINED) {
                notfound++;
            }
        }
        if (notfound > 0) {
            printf("notfound>0: %d \n", notfound);
        }
    } /* end else */

    free(ranks1);
    free(ranks2);

    MPI_Finalize();

    return 1;
}
