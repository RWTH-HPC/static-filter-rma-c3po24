/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/InvalidArg-ParrivedWithoutPartitionedRecvRequestError \
// RUN: 2>&1 \
// RUN: | %filecheck-may-segfault %s

// CHECK: [MUST-REPORT]{{.*The given request object does not correspond to a partitioned receive operation, which is erroneous.*}}

/**
 * @file InvalidArg-ParrivedWithoutPartitionedRecvRequestError.cpp
 * This is a test for the analysis group PartitionedP2PChecks.
 *
 * Description:
 * Performs a partitioned send and receive test.
 * Parrived is called with a partitioned send request instead of a recv request which is a usage error.
 *
 *  @date 13.09.2022
 *  @author Niko Sakic
 */

#include <iostream>
#include <mpi.h>
#define PARTITIONS 64
#define PARTLENGTH 16
#define MESSAGE_LENGTH PARTITIONS* PARTLENGTH
int main(int argc, char** argv)
{
    double message[MESSAGE_LENGTH];
    int send_partitions = PARTITIONS, send_partlength = PARTLENGTH;
    int source = 0, dest = 1, tag = 1, flag = 0;
    int size, rank;
    int i;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Request request;
    MPI_Info info = MPI_INFO_NULL;
    MPI_Request pp2psendrequest;

    MPI_Datatype send_type;
    MPI_Type_contiguous(send_partlength, MPI_DOUBLE, &send_type);
    MPI_Type_commit(&send_type);
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
            send_partitions,
            1,
            send_type,
            dest,
            tag,
            MPI_COMM_WORLD,
            info,
            &request);
        MPI_Start(&request);
        for (i = 0; i < send_partitions; ++i) {
            /* compute and fill partition #i, then mark ready: */
            MPI_Pready(i, request);
        }
        while (!flag) {
            /* do useful work #1 */
            MPI_Test(&request, &flag, MPI_STATUS_IGNORE);
            /* do useful work #2 */
        }
        MPI_Request_free(&request);
    }

    if (rank == 1) {
        MPI_Precv_init(
            message,
            send_partitions,
            send_partlength,
            MPI_DOUBLE,
            source,
            tag,
            MPI_COMM_WORLD,
            info,
            &request);
        MPI_Psend_init(
            message,
            send_partitions,
            1,
            send_type,
            dest,
            tag,
            MPI_COMM_WORLD,
            info,
            &pp2psendrequest);
        MPI_Start(&request);
        for (int j = 0; j < send_partitions; j += send_partlength) {
            while (!flag) {
                MPI_Parrived(pp2psendrequest, j, &flag);
                if (flag) {
                    // double a = message[j];
                    // printf("first value of partition %i is %f.\n",j,a);
                }
            }
            flag = 0;
        }
        MPI_Test(&request, &flag, MPI_STATUS_IGNORE);
        while (!flag) {
            /* do useful work #1 */
            MPI_Test(&request, &flag, MPI_STATUS_IGNORE);
            /* do useful work #2 */
        }
        MPI_Request_free(&request);
        MPI_Request_free(&pp2psendrequest);
    }

    //Say bye bye
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Type_free(&send_type);
    MPI_Finalize();

    return 0;
}
