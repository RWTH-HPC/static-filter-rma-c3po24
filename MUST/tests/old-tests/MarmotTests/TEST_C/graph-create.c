// RUN: %must-run %mpiexec-numproc-flag 4 \
// RUN: %must-bin-dir/marmot_c_graph-create 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are 1 communicators that are not freed when MPI_Finalize was issued}}

/**
 *  @file
 *
 *  This program is a very simple MPI program to verify that the 
 *  library works correctly.
 *  
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Finalize 
 *  - MPI_Graph_create
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: graph-create.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int nnodes = 0;
    int index[4] = {2, 3, 4, 6};
    int edges[6] = {1, 3, 0, 3, 0, 2};
    int size = -1;
    int rank = -1;
    int status = 0;

    MPI_Comm comm_graph = MPI_COMM_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (size < 4) {
        if (rank == 0)
            printf("This test needs at least 4 nodes! \n");
    } else {
        nnodes = 4;
        MPI_Graph_create(MPI_COMM_WORLD, nnodes, index, edges, 0, &comm_graph);
        if (rank < 4) {
            MPI_Topo_test(comm_graph, &status);
            if (status == MPI_GRAPH) {
                printf("New communicator is graph. \n");
            }
        }
    }

    MPI_Finalize();

    return 0;
}
