/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 4 \
// RUN: %must-bin-dir/IntegerGreaterCommSizeError 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error:.*Argument.*[(]nnodes[)].* specifies a rank that is greater then the size of the given communicator.}}

/**
 * @file IntegerGreaterCommSizeNoError.cpp
 * This is a a test for the analysis group CommChecks.
 *
 * Description:
 * Creates a graph communicator with 6 nodes in a communicator of just 4 nodes.
 * This will cause an error.
 *
 *
 *  @date 14.04.2011
 *  @author Mathias Korepkat, Tobias Hilbrich
 */

#include <iostream>
#include <mpi.h>

int main(int argc, char** argv)
{
    int size, rank;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    //Enough tasks ?
    if (size < 4) {
        std::cerr << "This test needs at least 4 processes!" << std::endl;
        MPI_Finalize();
        return 1;
    }

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    MPI_Comm comm1;
    int index[6] = {2, 3, 4, 6, 7, 8};
    int edges[8] = {1, 3, 0, 3, 0, 2, 4, 5};

    MPI_Graph_create(MPI_COMM_WORLD, 6, index, edges, 0, &comm1);
    if (comm1 != MPI_COMM_NULL)
        MPI_Comm_free(&comm1);

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
