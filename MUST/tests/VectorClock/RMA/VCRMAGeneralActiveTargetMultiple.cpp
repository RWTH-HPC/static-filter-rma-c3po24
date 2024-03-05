/* This file is part of GTI (Generic Tool Infrastructure)
 *
 * Copyright (C)
 *  2008-2019 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2013-2019 RWTH Aachen University, Federal Republic of Germany
 *
 * See the LICENSE file in the package base directory for details
 */

/**
 * @file VCRMAGeneralActiveTargetMultiple.cpp
 *       RMA general active target sync example
 *
 *  @date 05.06.2021
 *  @author Felix Tomski
 */

// RUN: %must-run --must:layout \
// RUN: %builddir/tests/VectorClock/VCRMAGeneralActiveTargetMultipleLayout.xml \
// RUN: %must-bin-dir/VCRMAGeneralActiveTargetMultiple 2>&1 \
// RUN: | %filecheck %s

// CHECK-DAG: shutdown(0){{.*}}clk=(3, 1, 1, 1, 1)
// CHECK-DAG: shutdown(1){{.*}}clk=(3, 5, 1, 3, 1)
// CHECK-DAG: shutdown(2){{.*}}clk=(3, 1, 5, 1, 1)
// CHECK-DAG: shutdown(3){{.*}}clk=(1, 1, 1, 3, 1)
// CHECK-DAG: shutdown(4){{.*}}clk=(3, 5, 5, 3, 3)

#include <mpi.h>
#include <iostream>
#include <unistd.h>

int main(int argc, char** argv)
{
    int size, rank, send_buf, recv_buf;
    MPI_Status status;
    MPI_Request request;
    MPI_Win win;
    MPI_Info info;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 5) {
        std::cerr << "This test needs at least five processes!" << std::endl;
        MPI_Finalize();
        return 0;
    }

    MPI_Group world_group;
    MPI_Comm_group(MPI_COMM_WORLD, &world_group);

    int win_buf;
    MPI_Win_create(&win_buf, sizeof(int), sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &win);
    MPI_Barrier(MPI_COMM_WORLD);

    // Pattern
    // P0 signals to P1 and P2
    // P1 receives from P0 and P3, signals to P4
    // P2 signals to P4
    // P3 signals to P1
    // P4 receives from P1 and P2
    if (rank == 0) {
        int destrank[2] = {1, 2};
        MPI_Group destgroup;
        MPI_Group_incl(world_group, 2, destrank, &destgroup);
        MPI_Win_start(destgroup, 0, win);
        MPI_Win_complete(win);
    } else if (rank == 1) {
        int srcrank[2] = {0, 3};
        MPI_Group srcgroup;
        MPI_Group_incl(world_group, 2, srcrank, &srcgroup);
        MPI_Win_post(srcgroup, 0, win);
        MPI_Win_wait(win);

        int destrank = 4;
        MPI_Group destgroup;
        MPI_Group_incl(world_group, 1, &destrank, &destgroup);
        MPI_Win_start(destgroup, 0, win);
        MPI_Win_complete(win);
    } else if (rank == 2) {
        int srcrank = 0;
        MPI_Group srcgroup;
        MPI_Group_incl(world_group, 1, &srcrank, &srcgroup);
        MPI_Win_post(srcgroup, 0, win);
        MPI_Win_wait(win);

        int destrank = 4;
        MPI_Group destgroup;
        MPI_Group_incl(world_group, 1, &destrank, &destgroup);
        MPI_Win_start(destgroup, 0, win);
        MPI_Win_complete(win);
    } else if (rank == 3) {
        int destrank = 1;
        MPI_Group destgroup;
        MPI_Group_incl(world_group, 1, &destrank, &destgroup);
        MPI_Win_start(destgroup, 0, win);
        MPI_Win_complete(win);
    } else if (rank == 4) {
        int srcrank[2] = {1, 2};
        MPI_Group srcgroup;
        MPI_Group_incl(world_group, 2, srcrank, &srcgroup);
        MPI_Win_post(srcgroup, 0, win);
        MPI_Win_wait(win);
    }

    // Do not call MPI_Win_free here to avoid synchronization of clocks
    // MPI_Win_free(&win);

    MPI_Finalize();
    return 0;
}
