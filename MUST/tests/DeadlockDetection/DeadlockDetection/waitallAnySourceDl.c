/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 3 %must-bin-dir/waitallAnySourceDl \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// RUN: %must-run-ddl %mpiexec-numproc-flag 3 \
// RUN: %must-bin-dir/DDlwaitallAnySourceDl 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*}}The application issued a set of MPI calls that can cause a deadlock!
// CHECK: {{(MPI_Waitall)|(MPI_Recv)}}

/**
 * @file waitallAnySourceDl.c
 * Deadlock test with an MPI_Waitall call that uses requests associated with a wildcard receive (Error).
 *
 * Description:
 * Deadlocking test with waitall and wildcard.
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>

int main(int argc, char** argv)
{
    int rank, size;
    MPI_Status statuses[3];
    MPI_Request requests[3];
    int buf[3];

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    //Enough tasks ?
    if (size < 3) {
        printf("This test needs at least 3 processes!\n");
        MPI_Finalize();
        return 1;
    }

    //Say hello
    printf("Hello, I am rank %d of %d processes.\n", rank, size);

    if (rank == 0) {
        MPI_Recv(&(buf[0]), 1, MPI_INT, 1, 666, MPI_COMM_WORLD, statuses);
    }

    if (rank == 1) {
        MPI_Irecv(
            &(buf[0]),
            1,
            MPI_INT,
            MPI_ANY_SOURCE,
            MPI_ANY_TAG,
            MPI_COMM_WORLD,
            &(requests[0]));
        MPI_Irecv(
            &(buf[1]),
            1,
            MPI_INT,
            MPI_ANY_SOURCE,
            MPI_ANY_TAG,
            MPI_COMM_WORLD,
            &(requests[1]));
        MPI_Irecv(
            &(buf[2]),
            2,
            MPI_INT,
            MPI_ANY_SOURCE,
            MPI_ANY_TAG,
            MPI_COMM_WORLD,
            &(requests[2]));
        MPI_Waitall(3, requests, statuses);
    }

    if (rank == 2) {
        MPI_Recv(&(buf[0]), 1, MPI_INT, 1, 666, MPI_COMM_WORLD, statuses);
    }

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
