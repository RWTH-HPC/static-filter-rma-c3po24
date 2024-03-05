/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/RequestNotKnownArrayNoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file RequestNotKnownArrayNoError.cpp
 * This is a a test for the analysis RequestCheck.
 *
 * Description:
 * Performs 3 irecvs and 3 sends with a waitall without any errors or warnings.
 *
 *
 *  @date 07.04.2011
 *  @author Mathias Korepkat
 */

#include <iostream>
#include <mpi.h>

int main(int argc, char** argv)
{
    int size, size1, size2, rank;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    size1 = size;
    size2 = size;
    MPI_Status status[3];
    MPI_Request request[3];
    //Enough tasks ?
    if (size < 2) {
        std::cerr << "This test needs at least 2 processes!" << std::endl;
        MPI_Finalize();
        return 1;
    }

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    if (rank == 1) {
        MPI_Irecv(&size, 1, MPI_INT, 0, 42, MPI_COMM_WORLD, request);
        MPI_Irecv(&size1, 1, MPI_INT, 0, 43, MPI_COMM_WORLD, &(request[1]));
        MPI_Irecv(&size2, 1, MPI_INT, 0, 44, MPI_COMM_WORLD, &(request[2]));
        MPI_Waitall(3, request, status);

        if (request[2] != MPI_REQUEST_NULL)
            MPI_Request_free(&(request[2]));
        if (request[1] != MPI_REQUEST_NULL)
            MPI_Request_free(&(request[1]));
        if (request[0] != MPI_REQUEST_NULL)
            MPI_Request_free(&(request[0]));
    }

    if (rank == 0) {
        MPI_Send(&size, 1, MPI_INT, 1, 42, MPI_COMM_WORLD);
        MPI_Send(&size, 1, MPI_INT, 1, 43, MPI_COMM_WORLD);
        MPI_Send(&size, 1, MPI_INT, 1, 44, MPI_COMM_WORLD);
    }

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
