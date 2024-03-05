/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/BufferOutsizeError \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT] Warning: from: call MPI_Bsend@1: Latest buffer usage of {{[0-9]+}} bytes outsized the attached buffer's size of {{[0-9]+}} bytes while {{[0-9]+}} bytes are already in use!

/**
 * @file BufferOutsizeError.cpp
 * This is a a test for the analysis BufferCheck.
 *
 * Description:
 * A buffer is attached to MPI, BufferedSend outsizing the buffer size, buffer detached
 * Should cause an outsized error
 * 
 *  @date 14.01.13
 *  @author Joachim Protze
 */

#include <iostream>
#include <mpi.h>

#define TEST_SEND_SIZE 1000000

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

    int* buffer = new int[TEST_SEND_SIZE];

    if (rank == 1) {
        int buffsize;
        MPI_Pack_size(TEST_SEND_SIZE, MPI_INT, MPI_COMM_WORLD, &buffsize);
        int* mpibuff = new int[(buffsize + MPI_BSEND_OVERHEAD - 1) / sizeof(int) + 1];
        MPI_Buffer_attach(mpibuff, buffsize + MPI_BSEND_OVERHEAD);
        MPI_Bsend(buffer, TEST_SEND_SIZE, MPI_INT, 0, 42, MPI_COMM_WORLD);
        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Bsend(buffer, TEST_SEND_SIZE, MPI_INT, 0, 42, MPI_COMM_WORLD);
        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Buffer_detach(&mpibuff, &buffsize);
        delete[] mpibuff;
    }

    if (rank == 0) {
        MPI_Recv(buffer, TEST_SEND_SIZE, MPI_INT, 1, 42, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Recv(buffer, TEST_SEND_SIZE, MPI_INT, 1, 42, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        MPI_Barrier(MPI_COMM_WORLD);
    }
    delete[] buffer;
    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
