// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_group-exclude-f 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Group_excl@.*: Argument . [(]ranks[)] is an array of ranks that must be in the given MPI group, the following entries do not match this criteria: ranks.*!}}

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
 *  $Id: group-exclude-f.c 319 2004-08-16 11:25:02Z rusbetti $  
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

    printf(" Here we use a non-existing rank \n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    num = size / 2;
    ranks = (int*)malloc(num * sizeof(MPI_INT));

    for (i = 0; i <= num - 2; i++) {
        ranks[i] = i;
    }
    ranks[num - 1] = size;

    printf(" I am rank %d of %d PEs\n", rank, size);

    MPI_Comm_group(MPI_COMM_WORLD, &worldgroup);
    MPI_Group_excl(worldgroup, num, ranks, &newgroup);
    /* MPI_Comm_create(comm_cart, worldgroup, &newcomm); */
    /* MPI_Comm_create(MPI_COMM_WORLD, worldgroup, &newcomm); */

    free(ranks);

    MPI_Finalize();

    return 1;
}
