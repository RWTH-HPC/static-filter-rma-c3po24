/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// UNSUPPORTED: fast-tests
// XFAIL: *
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/waitAnySourceEx2Dl \
// RUN: 2>&1 \
// RUN: | %filecheck --check-prefix 'CHECK-LOCAL' %s

// RUN: %must-run-ddl %mpiexec-numproc-flag 3 \
// RUN: %must-bin-dir/DDlwaitAnySourceEx2Dl 2>&1 \
// RUN: | %filecheck --check-prefix 'CHECK-DDL' %s

// CHECK-LOCAL: [MUST-REPORT] Information global: MUST issued a deadlock detection while a wildcard receive call (MPI_Recv/MPI_Irecv with MPI_ANY_SOURCE) was not yet completed.
// CHECK-LOCAL: [MUST-REPORT] Error global: The application issued a set of MPI calls that can cause a deadlock!{{.*MPI_Recv}}

// CHECK-DDL: [MUST-REPORT] Information global: MUST issued a deadlock detection while a wildcard receive call (MPI_Recv/MPI_Irecv with MPI_ANY_SOURCE) was not yet completed.
// CHECK-DDL: [MUST-REPORT] Error global: The application issued a set of MPI calls that can cause a deadlock!{{.*MPI_Recv}}

/**
 * @file waitAnySourceEx2Dl.c
 * Uses an irecv with a matching send that is not completed before a deadlock happens.
 * (This gives MUST a hard time, as it would give Umpire)
 *
 * Description:
 * Rather simple deadlock, but complication due to missing completion for anysource call. (No Error)
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>

int main(int argc, char** argv)
{
    int rank, size, buf;
    MPI_Status status;
    MPI_Request request;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    //Enough tasks ?
    if (size < 2) {
        printf("This test needs at least 2 processes!\n");
        MPI_Finalize();
        return 1;
    }

    //Say hello
    printf("Hello, I am rank %d of %d processes.\n", rank, size);

    if (rank == 0) {
        MPI_Isend(&size, 1, MPI_INT, 1, 666, MPI_COMM_WORLD, &request);

        MPI_Recv(&buf, 1, MPI_INT, 1, 777, MPI_COMM_WORLD, &status);

        MPI_Wait(&request, &status);
    }

    if (rank == 1) {
        MPI_Irecv(&size, 1, MPI_INT, MPI_ANY_SOURCE, 666, MPI_COMM_WORLD, &request);

        MPI_Recv(&buf, 1, MPI_INT, 0, 777, MPI_COMM_WORLD, &status);

        MPI_Wait(&request, &status);
    }

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
