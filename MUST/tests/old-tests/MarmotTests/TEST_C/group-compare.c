// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_group-compare 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are 3 groups that are not freed when MPI_Finalize was issued, a quality application should free all MPI resources before calling MPI_Finalize}}

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
 *  - MPI_Comm_group
 *  - MPI_Group_incl
 *  - MPI_Group_excl
 *  - MPI_Group_compare
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: group-compare.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int num = -1;
    int* ranks = 0;
    int i = 0;
    int result = -1;

    MPI_Group worldgroup;
    MPI_Group lowergroup;
    MPI_Group uppergroup;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    num = size / 2;
    ranks = (int*)malloc(num * sizeof(MPI_INT));

    for (i = 0; i <= num - 1; i++) {
        ranks[i] = i;
    }

    printf(" I am rank %d of %d PEs\n", rank, size);

    MPI_Comm_group(MPI_COMM_WORLD, &worldgroup);
    /* lowergroup contains lower half of ranks in COMM_WORLD */
    MPI_Group_incl(worldgroup, num, ranks, &lowergroup);
    /* uppergroup is disjunct to lowergroup an contains upper half of ranks */
    MPI_Group_excl(worldgroup, num, ranks, &uppergroup);
    MPI_Group_compare(lowergroup, uppergroup, &result);

    if (rank == 0) {
        switch (result) {
        case (MPI_IDENT):
            printf("Groups are ident \n ");
            break;
        case (MPI_SIMILAR):
            printf("Groups are similar \n ");
            break;
        case (MPI_UNEQUAL):
            printf("Groups are unequal \n ");
            break;
        default:
            /* do nothing. */
            break;
        }
    }

    free(ranks);

    MPI_Finalize();

    return 1;
}
