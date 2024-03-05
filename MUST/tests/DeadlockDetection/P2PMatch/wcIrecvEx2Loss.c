/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 4 %must-bin-dir/wcIrecvEx2Loss 2>&1 \
// RUN: | %filecheck --check-prefix 'CHECK-LOCAL' %s

// RUN: %must-run %mpiexec-numproc-flag 4 --must:layout \
// RUN: %builddir/tests/DeadlockDetection/DP2PMatch/wcIrecvEx2Losslayout.xml \
// RUN: %must-bin-dir/DwcIrecvEx2Loss 2>&1 \
// RUN: | %filecheck --check-prefix 'CHECK-LAYOUT' %s

// CHECK-LOCAL: {{(\[MUST-REPORT\] .*Error.*MPI_Isend@.*The application fails to match a point-to-point operation before it issues MPI_Finalize)|(\[MUST-RUNTIME\] .*MUST detected a deadlock)}}

// CHECK-LAYOUT: [MUST-REPORT]{{.*Error.*MPI_Isend@.*}}[[RANK:[0-9]]]{{.*}}Lost send of rank [[RANK]] to rank 0

/**
 * @file wcIrecvEx2Loss.c
 * Non-blocking test case with wildcards and a lost message (ERROR).
 *
 * Description:
 * There is a lost message in this test.
 * 1-3 send 4 messages to 0, which receive 3 messages with wildacrd and any tag from each rank
 * (or potentially more or less from some ranks, but 9 in total).
 * The remaining sends are lost. (ERROR)
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>

int main(int argc, char** argv)
{
    int rank, size, recv[9];
    MPI_Status stats[10];
    MPI_Request reqs[10];
    int i;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    //Enough tasks ?
    if (size < 4) {
        printf("This test needs at least 4 processes!\n");
        MPI_Finalize();
        return 1;
    }

    //Say hello
    printf("Hello, I am rank %d of %d processes.\n", rank, size);

    if (rank == 0) {
        for (i = 0; i < 9; i++) {
            MPI_Irecv(
                &(recv[i]),
                1,
                MPI_INT,
                MPI_ANY_SOURCE,
                MPI_ANY_TAG,
                MPI_COMM_WORLD,
                &(reqs[i]));
        }
        i = 9;
        MPI_Waitall(i, reqs, stats);
    }

    if (rank >= 1 && rank <= 3) {
        for (i = 0; i < 4; i++) {
            MPI_Isend(&size, 1, MPI_INT, 0, i, MPI_COMM_WORLD, &(reqs[i]));
            MPI_Request_free(&(reqs[i]));
        }
        //i = 3;

        /*We wait for the lost send here, this should not block for most MPIs as it is likely buffered*/
        //MPI_Wait (&(reqs[3]),stats);
    }

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
