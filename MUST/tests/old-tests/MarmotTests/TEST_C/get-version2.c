// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_get-version2 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

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
 *  - MPI_Get_version
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: get-version2.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int version = 0;
    int subversion = 0;
    int rank = -1;
    int size = -1;

    /* We call MPI_Get_version before MPI_Init. That is allowed. */
    MPI_Get_version(&version, &subversion);
    printf(
        "  MPI_Get_version called before MPI_Init. This is version %d.%d.\n",
        version,
        subversion);

    /* We call MPI_Get_version between MPI_Init and MPI_Finalize. */
    MPI_Init(&argc, &argv);
    MPI_Get_version(&version, &subversion);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf(
        " MPI_Get_version called between MPI_Init and MPI_Finalize. \
This is version %d.%d.\n I am rank %d of %d PEs.\n",
        version,
        subversion,
        rank,
        size);

    MPI_Finalize();

    /* We call MPI_Get_version after MPI_Finalize. That is allowed. */
    /*     MPI_Get_version(&version, &subversion); */
    /*     printf(" MPI_Get_version called after MPI_Finalize. \ */
    /* This is version %d.%d.\n", */
    /*            version, subversion); */

    /* We call MPI_Comm_rank and MPI_Comm_size after MPI_Finalize. */
    /* That is not allowed. */
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    printf(" I am rank %d of %d PEs.\n", rank, size);

    return 0;
}
