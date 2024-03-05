/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file 02_first_type_matching.c
 * A first type matching error.
 *
 * Description:
 * Process 0 builds a continuous datatype to send a message to process 1.
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>

#define COUNT 10

int main(int argc, char** argv)
{
    int rank, size, i, sum;
    int data[COUNT];
    MPI_Status status;
    MPI_Datatype contiType;

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

    //Create a contiguous datatype
    MPI_Type_contiguous(COUNT, MPI_INT, &contiType);
    MPI_Type_commit(&contiType);

    if (rank == 0) {
        //Prepare the buffer
        for (i = 0; i < COUNT; i++)
            data[i] = i;

        MPI_Send(data, 1, contiType, 1, 666, MPI_COMM_WORLD);
    } else if (rank == 1) {
        MPI_Recv(data, sizeof(int) * COUNT, MPI_BYTE, 0, 666, MPI_COMM_WORLD, &status);

        //Sum up what we received
        sum = 0;
        for (i = 0; i < COUNT; i++)
            sum += data[i];
        printf("Rank %d received %d integers with a sum of %d\n", rank, COUNT, sum);
    }

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
