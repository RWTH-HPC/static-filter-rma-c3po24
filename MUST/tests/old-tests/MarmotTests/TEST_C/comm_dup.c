// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_comm_dup \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-RUNTIME] ERROR: MUST detected a deadlock, detailed information is available in the MUST output file

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
 *  - MPI_Comm_dup
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: comm_dup.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int rank = -1;
    MPI_Comm newcomm;

    printf("Here we run into a deadlock because we call Comm_dup \
only on one node \n");

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    if (rank == 0) {
        printf(" I am rank %d of %d PEs\n", rank, size);
        MPI_Comm_dup(MPI_COMM_WORLD, &newcomm);
        printf(" I am rank %d of %d PEs\n", rank, size);
    } else {
    }

    MPI_Finalize();

    return 0;
}
