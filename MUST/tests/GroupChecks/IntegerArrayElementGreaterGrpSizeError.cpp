/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/IntegerArrayElementGreaterGrpSizeError 2>&1 \
// RUN: | %filecheck-may-segfault %s

// CHECK: [MUST-REPORT]{{.*Error:.*Argument.*[(]ranks1[)].*array of ranks that must be in the given MPI group.*entries do not match this criteria}}

/**
 * @file IntegerArrayElementGreaterGrpSizeError.cpp
 * This is a test for the analysis group GroupChecks.
 *
 * Description:
 * Performs a Group_translate_ranks call with a rank array argument that
 * contains an element that is greater then the size of the group.
 * This will cause an error.
 *
 * Positiv check is: GroupNotKnownNoError
 *
 *  @date 24.05.2011
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
    if (size < 2) {
        std::cerr << "This test needs at least 2 processes!" << std::endl;
        MPI_Finalize();
        return 1;
    }

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    //
    //create a new group
    MPI_Group group1, group2;
    MPI_Comm_group(MPI_COMM_WORLD, &group1);
    MPI_Comm_group(MPI_COMM_WORLD, &group2);

    int n = 2, ranksIn[2], ranksOut[2];
    ranksIn[0] = 2;
    ranksIn[1] = 3; // this is not allowed with a group size of 2

    MPI_Group_translate_ranks(group1, n, ranksIn, group2, ranksOut);

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
