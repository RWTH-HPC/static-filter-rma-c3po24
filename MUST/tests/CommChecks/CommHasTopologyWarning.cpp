/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/CommHasTopologyWarning 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning:.*Argument.*[(]comm_old[)].*is a communicator that already had a process topology}}

/**
 * @file CommHasTopologyWarning.cpp
 * This is a a test for the analysis group CommChecks.
 *
 * Description:
 * Creates a cartesian communicator with an old communicator that is already a
 * cartesian communicator. This causes a warning.
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

    //Enough tasks ?
    if (size < 2) {
        std::cerr << "This test needs at least 2 processes!" << std::endl;
        MPI_Finalize();
        return 1;
    }

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    //create a cartesian communicator
    MPI_Comm comm1, comm2;
    int dims[2], periods[2];
    dims[0] = 2;
    dims[1] = 1;
    periods[0] = 1;
    periods[1] = 1;

    MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, 0, &comm1);
    MPI_Cart_create(comm1, 2, dims, periods, 0, &comm2);

    if (comm1 != MPI_COMM_NULL)
        MPI_Comm_free(&comm1);
    if (comm2 != MPI_COMM_NULL)
        MPI_Comm_free(&comm2);

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
