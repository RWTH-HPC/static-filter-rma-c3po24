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
 * @file VCCollBlockingAllToAllOdd.cpp
 *       Blocking all-to-all communication example with odd number of ranks
 *
 *  @date 21.01.2024
 *  @author Simon Schwitanski
 */

// RUN: %must-run --must:layout \
// RUN: %builddir/tests/VectorClock/VCCollBlockingAllToAllOddLayout.xml \
// RUN: %must-bin-dir/VCCollBlockingAllToAllOdd 2>&1 \
// RUN: | %filecheck %s

// CHECK-DAG: shutdown(0){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(1){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(2){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(3){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(4){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(5){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(6){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(7){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(8){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(9){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(10){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(11){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
// CHECK-DAG: shutdown(12){{.*}}clk=(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

#include <iostream>
#include <mpi.h>
#include <unistd.h>

int main(int argc, char** argv)
{
    MPI_Init(&argc, &argv);

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Finalize();
    return 0;
}
