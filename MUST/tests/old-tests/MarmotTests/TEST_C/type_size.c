// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_type_size \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are 2 datatypes that are not freed when MPI_Finalize was issued}}

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
 *  - MPI_Type_size
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: type_size.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int size = -1;
    int typesize = -1;
    int typesize2 = -1;
    int typesize3 = -1;
    int rank = -1;

    MPI_Datatype int2 = MPI_DATATYPE_NULL;
    MPI_Datatype int_2 = MPI_DATATYPE_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    MPI_Type_size(MPI_INT, &typesize);
    printf("typesize=%d int=%lu \n", typesize, sizeof(int));

    MPI_Type_contiguous(2, MPI_INT, &int2);
    MPI_Type_size(int2, &typesize2);
    printf("typesize=%d 2int=%lu \n", typesize2, (2 * sizeof(int)));

    MPI_Type_vector(1, 2, 1, MPI_INT, &int_2);
    MPI_Type_size(int_2, &typesize3);
    printf("typesize=%d 2int=%lu \n", typesize3, (2 * sizeof(int)));

    MPI_Finalize();

    return 0;
}
