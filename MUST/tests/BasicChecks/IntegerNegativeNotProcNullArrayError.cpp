/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 3 \
// RUN: %must-bin-dir/IntegerNegativeNotProcNullArrayError 2>&1 \
// RUN: | %filecheck-may-segfault %s

// CHECK: [MUST-REPORT]{{.*Error:.*Argument.*[(]ranks1[)].*rank that must be in the given communicator or MPI_PROC_NULL}}

/**
 * @file IntegerNegativeNotProcNullArrayNoError.cpp
 * This is a test for the analysis group BasicChecks.
 *
 * Description:
 * Determing the relatve numbers of ranks in two Groups, using invalid negative ranks,
 * that causes error.
 *  @date 01.03.2011
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
    if (size < 3) {
        std::cerr << "This test needs at least 3 processes!" << std::endl;
        MPI_Finalize();
        return 1;
    }

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    if (rank == 0) {

        MPI_Group g1, g2;
        MPI_Comm_group(MPI_COMM_WORLD, &g1);
        MPI_Comm_group(MPI_COMM_WORLD, &g2);
        int ranks[3] = {0, 1, MPI_PROC_NULL};
        int ranks_out[3] = {0, 0, 0};

        ranks[2] = -1;
        while (ranks[2] == MPI_PROC_NULL)
            ranks[2]--;
        ranks[1] = -11;
        while (ranks[1] == MPI_PROC_NULL)
            ranks[1]--;

        MPI_Group_translate_ranks(g1, 3, ranks, g2, ranks_out);

        MPI_Group_free(&g1);
        MPI_Group_free(&g2);
    }

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
