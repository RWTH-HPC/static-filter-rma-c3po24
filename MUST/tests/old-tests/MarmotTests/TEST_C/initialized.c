// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_initialized \
// RUN: 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

/**
 *  @file
 *
 *  Simple test program.
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Finalize 
 *  - MPI_Initialized
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: initialized.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int flag = -1;

    MPI_Initialized(&flag);
    /* if (0 == flag) */
    printf(" MPI_Initialized called before MPI_Init ! \n");

    MPI_Init(&argc, &argv);
    MPI_Initialized(&flag);

    if (1 == flag) {
        printf(" MPI_Initialized called after MPI_Init ! \n");
    }

    MPI_Finalize();

    /* We call MPI_Initialized after MPI_Finalize. */
    MPI_Initialized(&flag);
    printf(" MPI_Initialized called after MPI_Finalize !\n");

    return 0;
}
