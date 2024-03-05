/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 4 \
// RUN: %must-bin-dir/IntegerEntryGreaterEqualArrayError 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error:.*Argument.*[(]edges[)].*, the following entries list higher node indices}}

/**
 * @file IntegerEntryGreaterEqualArrayError.cpp
 * This is a a test for the analysis group BasicChecks.
 *
 * Description:
 * Creates a graph communicator and uses an entry in edges array that
 * is higher then the number of nodes. This will cause an error.
 *
 *  @date 11.04.2011
 *  @author Mathias Korepkat
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
    int index[4] = {2, 3, 4, 6};
    int edges[6] = {1, 3, 4, 3, 5, 2}; /* the entries edges[2]=4; & edges[4]=5 will cause error */

    MPI_Graph_create(MPI_COMM_WORLD, 4, index, edges, 0, &comm1);

    MPI_Comm_free(&comm1);
    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
