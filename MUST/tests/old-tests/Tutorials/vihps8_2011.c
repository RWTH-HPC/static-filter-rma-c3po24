/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file vihps8_2011.c
 * Example application for 8th VI-HPS Tuning Workshop in September 2011.
 *
 * Description:
 * Contains multiple errors:
 * - Type missmatch
 * - Potential send-send deadlock
 * - Lost datatype
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>

int main(int argc, char** argv)
{
    int rank, size, sBuf[2] = {1, 2}, rBuf[2];
    MPI_Status status;
    MPI_Datatype newType;

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

    //1) Create a datatype
    MPI_Type_contiguous(2, MPI_INT, &newType);
    MPI_Type_commit(&newType);

    //2) Use MPI_Sendrecv to perform a ring communication
    MPI_Sendrecv(
        sBuf,
        1,
        newType,
        (rank + 1) % size,
        123,
        rBuf,
        sizeof(int) * 2,
        MPI_BYTE,
        (rank - 1 + size) % size,
        123,
        MPI_COMM_WORLD,
        &status);

    //3) Use MPI_Send and MPI_Recv to perform a ring communication
    MPI_Send(sBuf, 1, newType, (rank + 1) % size, 456, MPI_COMM_WORLD);
    MPI_Recv(
        rBuf,
        sizeof(int) * 2,
        MPI_BYTE,
        (rank - 1 + size) % size,
        456,
        MPI_COMM_WORLD,
        &status);

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}

/*EOF*/
