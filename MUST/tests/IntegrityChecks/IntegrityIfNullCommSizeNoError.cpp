/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/IntegrityIfNullCommSizeNoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file IntegrityIfNullCommSizeNoError.cpp
 * This is a a test for the analysis group BasicChecks.
 *
 * Description:
 * Performs a MPI_Gatherv, without triggering any errors or warnings
 *
 *  @date 30.05.2011
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

    int counts[2] = {2, 2}, displs[2] = {1, 3};
    char sendbuf[7] = {'I', 'T', 'W', 'o', 'r', 'k', 's'},
         recvbuf[7] = {'I', 'T', 'W', 'o', 'r', 'k', 's'};

    MPI_Gatherv(
        sendbuf,
        counts[rank],
        MPI_CHAR,
        recvbuf,
        counts,
        displs,
        MPI_CHAR,
        0,
        MPI_COMM_WORLD);

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
