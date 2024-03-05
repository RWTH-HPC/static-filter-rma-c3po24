/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/RequestActiveRecvWarning 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning:.*Argument.*[(]request[)].*is an active receive request, that should never be freed}}

/**
 * @file RequestActiveRecvWarning.cpp
 * This is a a test for the analysis RequestCheck.
 *
 * Description:
 * Performs a MPI_Send_init/MPI_Recv_init, MPI_Start() and do a MPI_Request_free on the activ request of recv.
 * That is allowed but suspicious.
 *
 *
 *  @date 08.04.2011
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

    MPI_Request request;
    //Enough tasks ?
    if (size < 2) {
        std::cerr << "This test needs at least 2 processes!" << std::endl;
        MPI_Finalize();
        return 1;
    }

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    if (rank == 0) {
        MPI_Send_init(&size, 1, MPI_INT, 1, 42, MPI_COMM_WORLD, &request);
        MPI_Start(&request);
    }

    if (rank == 1) {
        MPI_Recv_init(&size, 1, MPI_INT, 0, 42, MPI_COMM_WORLD, &request);
        MPI_Start(&request);
        MPI_Request_free(&request);
    }
    if (request != MPI_REQUEST_NULL)
        MPI_Request_free(&request);

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
