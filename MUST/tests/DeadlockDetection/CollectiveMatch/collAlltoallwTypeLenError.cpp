/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: HAVE_MPI_ALLTOALLW
// RUN: %must-run %mpiexec-numproc-flag 3 \
// RUN: %must-bin-dir/collAlltoallwTypeLenError 2>&1 \
// RUN: | %filecheck-may-segfault --check-prefix 'CHECK-LOCAL' %s

// RUN: %must-run %mpiexec-numproc-flag 3 --must:layout \
// RUN: %builddir/tests/DeadlockDetection/DCollectiveMatch/collAlltoallwTypeLenErrorlayout.xml \
// RUN: %must-bin-dir/DcollAlltoallwTypeLenError 2>&1 \
// RUN: | %filecheck-may-segfault --check-prefix 'CHECK-DISTRIBUTED-LAYOUT' %s

// RUN: %must-run %mpiexec-numproc-flag 3 --must:layout \
// RUN: %builddir/tests/DeadlockDetection/DCollectiveMatch/DIntracollAlltoallwTypeLenErrorlayout.xml \
// RUN: %must-bin-dir/DIntracollAlltoallwTypeLenError 2>&1 \
// RUN: | %filecheck-may-segfault --check-prefix 'CHECK-DISTRIBUTED-INTRA' %s

// CHECK-LOCAL: [MUST-REPORT]{{.*Two collective calls use [(]datatype,count[)] pairs that span type signatures of different length}}

// CHECK-DISTRIBUTED-LAYOUT: [MUST-REPORT]{{.*Two collective .* use [(]datatype,count[)] pairs that span type signatures of different length}}

// CHECK-DISTRIBUTED-INTRA: [MUST-REPORT]{{.*Two collective .* use [(]datatype,count[)] pairs that span type signatures of different length}}

/**
 * @file collAlltoallwTypeLenError.cpp
 * Collective matching test.
 *
 * Description:
 * Performs a MPI_Alltoallw collective with an error
 *
 *
 *  @date 27.03.2012
 *  @author Joachim Protze
 */

#include <iostream>
#include <mpi.h>

int main(int argc, char** argv)
{
    int size, rank, i;
    int inbuf[200], outbuf[200];

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size != 3) {
        if (rank == 0)
            std::cout << "This test needs 3 processes" << std::endl;
        MPI_Finalize();
        return 1;
    }

    for (i = 0; i < 100; i++)
        outbuf[i] = rank * 100 + i;

    MPI_Datatype contis[3];

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    int recvcnts[3] = {18, 12, 6};
    int sendcnts[3] = {18, 12, 6};
    int displs[3] = {0 * sizeof(int), 48 * sizeof(int), 96 * sizeof(int)};
    int typesize;

    for (i = 0; i < 3; i++) {
        typesize = (2 + rank + i) % 3 + 1;
        MPI_Type_contiguous(typesize, MPI_INT, contis + i);
        MPI_Type_commit(contis + i);
        recvcnts[i] = ((3 + rank - i) % 3 + 1 + rank % 2) * 6 / typesize;
        sendcnts[i] = ((3 - rank + i) % 3 + 1) * 6 / typesize;
    }
    MPI_Alltoallw(
        outbuf,
        sendcnts,
        displs,
        contis,
        inbuf,
        recvcnts,
        displs,
        contis,
        MPI_COMM_WORLD);

    for (i = 0; i < 3; i++) {
        MPI_Type_free(contis + i);
    }
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
