/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/IntegerZeroArrayWarning 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning[:] from[:] call MPI_Gatherv@.*0.*[:] Argument 5 [(]recvcounts[)] is an array that contains zero value[(]s[)], which is correct but unusual}}

/**
 * @file IntegerZeroArrayWarning.cpp
 * This is a a test for the analysis group BasicChecks.
 *
 * Description:
 * Performs a Gatherv with a 0 entry in recvCount array,
 * which is allowed but suspicious (Will cause that a process get no
 * Data).
 *
 *  @date 04.03.2011
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
    if (size < 2) {
        std::cerr << "This test needs at least 2 processes!" << std::endl;
        MPI_Finalize();
        return 1;
    }

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    int inBuff = 1,
        recvCounts[2] = {0, 1}; //a 0 in recv Count cause a warrning because it is unusual use.
    int outBuff[2] = {0, 0};
    int recvDispl[2] = {0, 1};

    MPI_Gatherv(
        &inBuff,
        1,
        MPI_INT,
        outBuff,
        recvCounts /* this array containing a 0 what is unusual use */,
        recvDispl,
        MPI_INT,
        0,
        MPI_COMM_WORLD);

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
