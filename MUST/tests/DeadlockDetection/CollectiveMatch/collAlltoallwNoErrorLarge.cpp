/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: HAVE_MPI_ALLTOALLW
// RUN: %must-run %mpiexec-numproc-flag 8 \
// RUN: %must-bin-dir/collAlltoallwNoErrorLarge 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

// RUN: %must-run %mpiexec-numproc-flag 8 --must:layout \
// RUN: %builddir/tests/DeadlockDetection/DCollectiveMatch/collAlltoallwNoErrorLargelayout.xml \
// RUN: %must-bin-dir/DcollAlltoallwNoErrorLarge 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

// RUN: %must-run %mpiexec-numproc-flag 8 --must:layout \
// RUN: %builddir/tests/DeadlockDetection/DCollectiveMatch/DIntracollAlltoallwNoErrorLargelayout.xml \
// RUN: %must-bin-dir/DIntracollAlltoallwNoErrorLarge 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file collAlltoallwNoErrorLarge.cpp
 * Collective matching test.
 *
 * Description:
 * Performs a MPI_Alltoallw collective with no error
 *
 *  @date 27.03.2012
 *  @author Joachim Protze
 */

#include <iostream>
#include <mpi.h>

int main(int argc, char** argv)
{
    int size, rank, i;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 3) {
        if (rank == 0)
            std::cout << "This test needs at least 3 processes" << std::endl;
        MPI_Finalize();
        return 1;
    }

    //Say hello
    std::cout << "Hello, I am rank " << rank << " of " << size << " processes." << std::endl;

    MPI_Datatype* contis;
    int *inbuf, *outbuf, *displs, *recvcnts, *sendcnts;
    inbuf = new int[size * 24];
    outbuf = new int[size * 24];
    contis = new MPI_Datatype[size];
    displs = new int[size];
    recvcnts = new int[size];
    sendcnts = new int[size];

    int typesize;

    for (i = 0; i < size * 24; i++)
        outbuf[i] = rank * size * 24 + i;

    for (i = 0; i < size; i++) {
        typesize = (2 + rank + i) % 3 + 1;
        MPI_Type_contiguous(typesize, MPI_INT, contis + i);
        MPI_Type_commit(contis + i);
        recvcnts[i] = ((size + rank - i) % 3 + 1) * 6 / typesize;
        sendcnts[i] = ((size - rank + i) % 3 + 1) * 6 / typesize;
        displs[i] = 24 * i * sizeof(int);
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

    for (i = 0; i < size; i++) {
        MPI_Type_free(contis + i);
    }
    std::cout << "Signing off, rank " << rank << "." << std::endl;

    MPI_Finalize();

    return 0;
}
