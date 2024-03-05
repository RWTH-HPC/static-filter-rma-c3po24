/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file welcome.c
 * A simple example to demonstrate ISP.
 *
 * Description:
 * For all processes, process i sends a message to process (i+1)%size.
 * The communication contains an error.
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>

int main(int argc, char** argv)
{
    int rank, size, sBuf, rBuf, left, right;
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

    //Set neighbors
    right = (rank + 1) % size;
    left = ((rank - 1) + size) % size; //we add "size" due to the modulo in C being weird

    //Receive from left neighbor
    MPI_Recv(&rBuf, 1, MPI_INT, left, 123, MPI_COMM_WORLD, &status);
    printf("Rank %d got: %d.\n", rank, rBuf);

    //Send to right neighbor
    sBuf = rank;
    printf("Rank %d sends to %d, message: %d.\n", rank, right, sBuf);
    MPI_Isend(&sBuf, 1, MPI_INT, right, 123, MPI_COMM_WORLD, &request);
    MPI_Wait(&request, &status);

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}

/*EOF*/
