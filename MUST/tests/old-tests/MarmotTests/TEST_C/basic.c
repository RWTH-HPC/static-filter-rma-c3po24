// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_basic 2>&1 \
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
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: basic.c 309 2004-08-11 11:12:56Z rusbetti $  
 */

#include <mpi.h>

int main(int argc, char** argv)
{
    MPI_Init(&argc, &argv);

    MPI_Finalize();

    return 0;
}
