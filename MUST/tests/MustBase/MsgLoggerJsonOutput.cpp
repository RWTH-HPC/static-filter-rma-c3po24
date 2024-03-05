/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: cd %T && rm -f %T/MUST_Output.json && \
// RUN:   %must-run %mpiexec-numproc-flag 1 --must:stacktrace none \
// RUN:   --must:output json %must-bin-dir/MsgLoggerJsonOutput 2>&1 \
// RUN:   && diff %S/MsgLoggerJsonOut.json %t/MUST_Output.json

/**
 * @file MsgLoggerJsonOut.cpp
 * This is a a test for the json message logger.
 *
 * Description:
 * Checks
 *
 * This test is a bit crude. I tried not to introduce a new dependency on some JSON validator. It
 * will definitely break on changing the json output. Sorry for that!
 *
 */

#include <iostream>
#include <mpi.h>

int main(int argc, char** argv)
{
    int size, rank;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    //create a cartesian communicator
    MPI_Comm comm;
    int dims[3], periods[3];
    dims[0] = size;
    dims[1] = 1;
    dims[2] = 1;
    periods[0] = 1;
    periods[1] = 1;
    periods[2] = 1;

    //!!! Warning happens here
    MPI_Cart_create(MPI_COMM_WORLD, 3, dims, periods, -1, &comm);
    MPI_Comm_free(&comm);

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
