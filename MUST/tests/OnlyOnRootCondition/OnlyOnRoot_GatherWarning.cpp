/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/OnlyOnRoot_GatherWarning 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Warning:.*MPI_Gather@.*1: Argument 5 \
// RUN: [(]recvcount[)] is zero, which is correct but unusual!)}}' %s

// CHECK: [MUST-REPORT]{{.*Warning:.*MPI_Gather@.*0: Argument 5 [(]recvcount[)] is zero, which is correct but unusual!}}

/**
 * @file OnlyOnRoot_Gather.cpp
 * This is a a test for the preconditioner of OnlyOnRootCondition for MPI_Gatherv.
 *
 * Description:
 * Performs a Gather with a 0 entry in recvCount,
 * which is allowed but suspicious. This is just significant at root.
 *
 *  @date 23.04.2011
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

    int inBuff = 1;
    int outBuff[2] = {0, 0};

    MPI_Gather(&inBuff, 0, MPI_INT, outBuff, 0, MPI_INT, 0, MPI_COMM_WORLD);

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
