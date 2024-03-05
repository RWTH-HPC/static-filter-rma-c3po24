/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/GroupIsEmptyNoWarning 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file GroupIsEmptyNoWarning.cpp
 * This is a test for the analysis group GroupChecks.
 *
 * Description:
 * Creates a communicator with the help of Comm_create call, without triggering any errors or warnings
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
    MPI_Group group;
    MPI_Comm_group(MPI_COMM_WORLD, &group);

    //create a communicator
    MPI_Comm_create(MPI_COMM_WORLD, group, &newcomm);

    //free group and communicator
    if (newcomm != MPI_COMM_NULL)
        MPI_Comm_free(&newcomm);
    if (group != MPI_GROUP_NULL)
        MPI_Group_free(&group);
    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
