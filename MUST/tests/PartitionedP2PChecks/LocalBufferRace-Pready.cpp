/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: partitioned-race-support

// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/LocalBufferRace-Pready \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// RUN: mustrun -np 2 %s.exe 2>&1 | %filecheck %s

// CHECK: data race

/**
 * @file LocalBufferRace-Pready.cpp
 * This is a test for the analysis group PartitionedP2PChecks.
 *
 * Description:
 * Performs partitioned communication with a local buffer access *after*
 * Pready has marked the partitins to be ready which a data race.
 *
 *  @date 13.09.2022
 *  @author Niko Sakic
 */

#include <atomic>
#include <iostream>
#include <mpi.h>
#include <vector>
#include <unistd.h>
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

    if (rank == 0) {
        for (int i = 0; i < partitions; i++) {
            message[i] = i + 1;
        }
    }

    if (rank == 1) {
        for (int i = 0; i < partitions; i++) {
            message[i] = -1;
        }
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
        for (i = 0; i < partitions; ++i) {
            /* compute and fill partition #i, then mark ready: */
            MPI_Pready(i, request);
        }
        message[0] = 42; // data race
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
        printf("message[0] is %f\n", message[0]);
        MPI_Request_free(&request);
    }

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
