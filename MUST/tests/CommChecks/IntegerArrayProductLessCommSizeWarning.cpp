/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/IntegerArrayProductLessCommSizeWarning 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning:.*Argument.*[(]dims[)].*specifies a cartesian layout that uses less ranks than the given communicator}}

/**
 * @file IntegerArrayProductLessCommSizeWarning.cpp
 * This is a a test for the analysis group CommChecks.
 *
 * Description:
 * Creates a cartesian communicator with 3 ranks but there are only 2
 * in the communicator, so this will cause an error.
 *
 * Positiv check is: IntegerArrayProductGreaterCommSizeNoError
 *
 *  @date 15.04.2011
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
    MPI_Comm comm;
    int dims[2], periods[2];
    dims[0] = 1;
    dims[1] = 1; // there are only 2 processes in MPI_COMM_WORLD
    periods[1] = 1;
    periods[0] = 1;

    MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, 0, &comm);
    if (comm != MPI_COMM_NULL)
        MPI_Comm_free(&comm);

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
