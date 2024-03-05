/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file waitallAnySourceEx1Dl.c
 * Deadlock with missing information for a wild-card receive.
 *
 * Description:
 * Rather simple deadlock, but complication due to missing completion for anysource call.
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>

int main(int argc, char** argv)
{
    int rank, size, buf;
    MPI_Status statuses[2];
    MPI_Request requests[2];

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
        MPI_Send(&size, 1, MPI_INT, 1, 666, MPI_COMM_WORLD);
    }

    if (rank == 1) {
        MPI_Irecv(&size, 1, MPI_INT, MPI_ANY_SOURCE, 666, MPI_COMM_WORLD, &(requests[0]));
        MPI_Irecv(&size, 1, MPI_INT, 0, 666, MPI_COMM_WORLD, &(requests[1]));

        MPI_Waitall(2, requests, statuses);
    }

    if (rank == 2) {
        sleep(2);
        MPI_Send(&size, 1, MPI_INT, 1, 666, MPI_COMM_WORLD);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
