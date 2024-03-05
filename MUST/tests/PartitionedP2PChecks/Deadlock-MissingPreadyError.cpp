/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: partitioned-deadlock-support

// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/Deadlock-MissingPreadyError \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*}}The application issued a set of MPI calls that can cause a deadlock!

/**
 * @file Deadlock-MissingPReadyError.cpp
 * This is a test for the analysis group PartitionedP2PChecks.
 *
 * Description:
 * A deadlock in partitioned communication due to a missing Pready call
 * on rank 0 (partition 0 is not activated).
 *
 *  @date 13.09.2022
 *  @author Niko Sakic, Simon Schwitanski
 */

#include <atomic>
#include <iostream>
#include <mpi.h>
#include <vector>

#define PARTITIONS 8
#define COUNT 5

int main(int argc, char** argv)
{
    double message[PARTITIONS * COUNT];
    MPI_Count partitions = PARTITIONS;
    int size, rank;
    int source = 0, dest = 1, tag = 1, flag = 0;
    int i;
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
        MPI_Psend_init(
            message,
            partitions,
            COUNT,
            MPI_DOUBLE,
            dest,
            tag,
            MPI_COMM_WORLD,
            MPI_INFO_NULL,
            &request);
        MPI_Start(&request);
        for (i = 1; i < partitions; ++i) {
            /* compute and fill partition #i, then mark ready: */
            MPI_Pready(i, request);
        }
        MPI_Wait(&request, MPI_STATUS_IGNORE);
        MPI_Request_free(&request);
    }

    if (rank == 1) {
        MPI_Precv_init(
            message,
            partitions,
            COUNT,
            MPI_DOUBLE,
            source,
            tag,
            MPI_COMM_WORLD,
            MPI_INFO_NULL,
            &request);
        MPI_Start(&request);
        MPI_Wait(&request, MPI_STATUS_IGNORE);
        MPI_Request_free(&request);
    }

    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
