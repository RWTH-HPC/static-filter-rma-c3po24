// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_comm-create-f 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Comm_create@.*: Argument 2 [(]group[)] is a group which should be a subset of argument 1[(]comm[)], but the following ranks are in the group but not in the communicator}}

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
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: comm-create-f.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int num = 0;
    int* ranks = 0;
    int i = 0;

    MPI_Comm lowcomm;
    MPI_Comm falsecomm;

    MPI_Group worldgroup;
    MPI_Group lowgroup;
    MPI_Group uppergroup;

    printf("We call Comm_create twice:\n");
    printf(" 1: correctly, 2: ranks of old comm and group are distinct \n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    num = size / 2;
    ranks = (int*)malloc(num * sizeof(MPI_INT));
    /*     printf("num: %d \n", num); */
    for (i = 0; i < num; i++) {
        ranks[i] = i;
    }

    printf(" I am rank %d of %d PEs \n", rank, size);

    MPI_Comm_group(MPI_COMM_WORLD, &worldgroup);
    MPI_Group_incl(worldgroup, num, ranks, &lowgroup);
    MPI_Group_excl(worldgroup, size - num, ranks, &uppergroup);
    MPI_Comm_create(MPI_COMM_WORLD, lowgroup, &lowcomm);

    if (lowcomm != MPI_COMM_NULL) {
        MPI_Comm_create(lowcomm, uppergroup, &falsecomm);
    }

    free(ranks);

    MPI_Finalize();

    return 1;
}
