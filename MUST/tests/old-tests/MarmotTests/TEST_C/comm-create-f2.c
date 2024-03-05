// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_comm-create-f2 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are . groups that are not freed when MPI_Finalize was issued,}}

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
 *  $Id: comm-create-f2.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    int ranks1 = -1;
    int ranks2 = 1;

    MPI_Comm falsecomm;
    MPI_Comm uppercomm;

    MPI_Group worldgroup;
    MPI_Group lowgroup;
    MPI_Group uppergroup;

    printf("We call Comm_create twice:\n"
           "1: correctly, 2: group s not a subset of communicator \n");
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    ranks1 = size - 1;

    printf(" I am rank %d of %d PEs\n", rank, size);

    MPI_Comm_group(MPI_COMM_WORLD, &worldgroup);
    MPI_Group_excl(worldgroup, 1, &ranks1, &lowgroup);
    MPI_Group_excl(worldgroup, 1, &ranks2, &uppergroup);
    MPI_Comm_create(MPI_COMM_WORLD, uppergroup, &uppercomm);

    if (uppercomm != MPI_COMM_NULL) {
        MPI_Comm_create(uppercomm, lowgroup, &falsecomm);
    }

    MPI_Finalize();

    return 1;
}
