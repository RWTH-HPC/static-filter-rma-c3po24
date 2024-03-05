/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/CommIsPredefinedError 2>&1 \
// RUN: | %filecheck-may-segfault %s

// CHECK: [MUST-REPORT]{{.*Error:.*Argument.*[(]comm[)].*is a predefined communicator, which must not be freed}}

/**
 * @file CommIsPredefinedError.cpp
 * This is a test for the analysis group  CommChecks.
 *
 * Description:
 * Creates a communicator with MPI_Comm_split and frees it. Afterards free MPI_COMM_WORLD,
 * what causes an error.
 *
 *  @date 28.04.2011
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

    MPI_Comm localcomm, comm_world;
    int color;

    //Enough tasks ?
    if (size < 2) {
        std::cerr << "This test needs at least 2 processes!" << std::endl;
        MPI_Finalize();
        return 1;
    }

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    //create a second intracommunicator
    color = rank % 2;
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, &localcomm);

    MPI_Comm_free(&localcomm);

    comm_world = MPI_COMM_WORLD;
    MPI_Comm_free(&comm_world);

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
