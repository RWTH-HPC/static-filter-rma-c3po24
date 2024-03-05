// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_group-exclude 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are 2 groups that are not freed when MPI_Finalize was issued, a quality application should free all MPI resources}}

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
 *  - MPI_Group_excl
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: group-exclude.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int num = 0;
    int* ranks = 0;
    int i = 0;

    MPI_Group worldgroup;
    MPI_Group newgroup;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    num = size / 2;
    ranks = (int*)malloc(num * sizeof(MPI_INT));

    for (i = 0; i < num; i++) {
        ranks[i] = i;
    }

    printf(" I am rank %d of %d PEs\n", rank, size);

    MPI_Comm_group(MPI_COMM_WORLD, &worldgroup);
    MPI_Group_excl(worldgroup, num, ranks, &newgroup);

    free(ranks);
    MPI_Finalize();

    return 1;
}
