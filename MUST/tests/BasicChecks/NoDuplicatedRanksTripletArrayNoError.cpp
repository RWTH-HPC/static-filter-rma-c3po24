/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 4 \
// RUN: %must-bin-dir/NoDuplicatedRanksTripletArrayNoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file NoDuplicatedRanksTripletArrayNoError.cpp
 * This is a test for the analysis group BasicChecks.
 *
 * Description:
 * Creates a new group with the help of the call MPI_Group_range_excl,
 * without triggering any errors or warnings
 *
 *  @date 24.05.2011
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

    MPI_Group group1, group2;
    MPI_Comm_group(MPI_COMM_WORLD, &group1);

    int n = 2;
    int range[2][3] = {{0, 3, 2}, {3, 0, -2}};
    MPI_Group_range_excl(group1, n, range, &group2);

    //free groups
    if (group1 != MPI_GROUP_NULL) {
        MPI_Group_free(&group1);
    }

    if (group2 != MPI_GROUP_NULL) {
        MPI_Group_free(&group2);
    }

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
