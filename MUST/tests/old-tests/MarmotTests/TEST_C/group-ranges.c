// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_group-ranges 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Group_range_incl@.*: Argument 3 [(]ranges[)] is an array of triplets of the form}}

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
 *  - MPI_Group_range_incl
 *  - MPI_Group_range_excl
 *  - MPI_Group_free
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: group-ranges.c 997 2009-10-02 14:44:28Z hpczink $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

/* const int NUM = 3; */
enum { NUM = 3 };

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int ranges[NUM][3];
    int ranges1[NUM][3];
    int i = 0;

    MPI_Group worldgroup;
    MPI_Group newgroup;

    printf("This programm works correctly for less than 10 nodes. \n");
    printf("With 10 nodes or more we try to exclude some ranks twice. \n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    for (i = 0; i <= NUM - 1; i++) {
        /* ranges for range_incl, don't have to be distinct */
        ranges[i][0] = 0;
        ranges[i][1] = size - 1;
        ranges[i][2] = (i + 2);
        /* ranges for range_excl, are erroneous if not distinct */
        ranges1[i][0] = 0 + i;
        ranges1[i][1] = size - 1;
        ranges1[i][2] = (i + 3);
    }

    printf(" I am rank %d of %d PEs\n", rank, size);

    MPI_Comm_group(MPI_COMM_WORLD, &worldgroup);
    MPI_Group_range_incl(worldgroup, NUM, ranges, &newgroup);
    MPI_Group_free(&newgroup);
    MPI_Group_range_excl(worldgroup, NUM, ranges1, &newgroup);
    MPI_Group_free(&newgroup);

    MPI_Finalize();

    return 1;
}
