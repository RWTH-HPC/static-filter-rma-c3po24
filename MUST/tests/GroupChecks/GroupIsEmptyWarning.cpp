/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/GroupIsEmptyWarning \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning:.*Argument.*[(]group[)].*is an empty group, which is allowed but unusual}}

/**
 * @file GroupIsEmptyWarning.cpp
 * This is a test for the analysis group GroupChecks.
 *
 * Description:
 * Creates a communicator with the help of Comm_create call and uses a
 * empty group, what will cause an error.
 *
 *
 *  @date 23.05.2011
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
    MPI_Comm newcomm;
    MPI_Group group_empty;
    group_empty = MPI_GROUP_EMPTY;

    //create a communicator
    MPI_Comm_create(MPI_COMM_WORLD, group_empty, &newcomm);

    //free group and communicator
    if (newcomm != MPI_COMM_NULL)
        MPI_Comm_free(&newcomm);

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
