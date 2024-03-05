// RUN: %must-run %mpiexec-numproc-flag 4 %must-bin-dir/marmot_c_graphs 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are 1 communicators that are not freed when MPI_Finalize was issued}}

/**
 *  @file
 *
 *  This program is a very simple MPI program to verify that the 
 *  library works correctly.
 *  It's the example mentioned in the MPI-Standard.
 *
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Finalize 
 *  - MPI_Graph_create
 *  - MPI_Graphdims_get
 *  - MPI_Graph_get
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: graphs.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

int main(int argc, char** argv)
{
    int nnodes = 0;
    int index[4] = {2, 3, 4, 6};
    int edges[6] = {1, 3, 0, 3, 0, 2};
    int size = 0;
    int rank = -1;
    int maxindex = -1;
    int maxedges = -1;
    int* new_index = 0;
    int* new_edges = 0;

    MPI_Comm comm_graph = MPI_COMM_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (size < 4) {
        if (rank == 0) {
            printf("This test needs at least 4 nodes! \n");
        }
    } else {
        nnodes = 4;
        MPI_Graph_create(MPI_COMM_WORLD, nnodes, index, edges, 0, &comm_graph);
        if (comm_graph != MPI_COMM_NULL) {
            MPI_Graphdims_get(comm_graph, &maxindex, &maxedges);
            new_index = (int*)malloc(sizeof(int) * maxindex);
            new_edges = (int*)malloc(sizeof(int) * maxedges);
            MPI_Graph_get(comm_graph, maxindex, maxedges, new_index, new_edges);
            free(new_index);
            free(new_edges);
        }
    }

    MPI_Finalize();

    return 0;
}
