// RUN: %must-run %mpiexec-numproc-flag 6 \
// RUN: %must-bin-dir/marmot_c_group-translate-ranks 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are 3 groups that are not freed when MPI_Finalize was issued}}

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
 *  - MPI_Comm_create
 *  - MPI_Group_translate_ranks
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: group-translate-ranks.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int undef = 0;
    int* ranks = 0;
    int* ranks1 = 0;
    int* ranks2 = 0;
    int num = -1;
    int num2 = -1;
    int i = 0;

    MPI_Group worldgroup;
    MPI_Group lowergroup;
    MPI_Group uppergroup;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    num = size / 2;
    ranks = (int*)malloc(num * sizeof(int));

    for (i = 0; i <= num - 1; i++) {
        ranks[i] = i;
    }

    printf(" I am rank %d of %d PEs\n", rank, size);

    MPI_Comm_group(MPI_COMM_WORLD, &worldgroup);
    /* lowergroup contains lower half of ranks in COMM_WORLD */
    MPI_Group_incl(worldgroup, num, ranks, &lowergroup);
    /* uppergroup is disjunct to lowergroup an contains upper half of ranks */
    MPI_Group_excl(worldgroup, num, ranks, &uppergroup);
    MPI_Group_size(lowergroup, &num2);

    ranks1 = (int*)malloc(num2 * sizeof(int));
    ranks2 = (int*)malloc(num2 * sizeof(int));

    for (i = 0; i <= num2 - 1; i++) {
        ranks1[i] = i;
        ranks2[i] = 0;
    }

    MPI_Group_translate_ranks(lowergroup, num2, ranks1, uppergroup, ranks2);

    for (i = 0; i <= num2 - 1; i++) {
        if (ranks2[i] == MPI_UNDEFINED) {
            undef++;
        }
    }

    if (rank == 0) {
        printf(" -> %d ranks not in second group! \n", undef);
    }

    free(ranks);
    free(ranks1);
    free(ranks2);

    MPI_Finalize();

    return 1;
}
