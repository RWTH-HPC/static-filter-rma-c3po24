// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 1 %must-bin-dir/marmot_c_abort 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Abort@.*: Argument 1 [(]comm[)] is MPI_COMM_NULL, which is allowed but unusual}}

/**
 *  @file
 *
 *  This program is a very simple MPI program to verify that the 
 *  library works correctly.
 *
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Abort
 *  - MPI_Finalize 
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: abort.c 402 2005-09-08 11:59:32Z rusbetti $  
 */

#include <mpi.h>

int main(int argc, char** argv)
{
    MPI_Init(&argc, &argv);

    MPI_Abort(MPI_COMM_NULL, 42);

    MPI_Finalize();

    return 0;
}
