/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file 02_can_deadlock.c
 * Potential deadlock that is schedule dependent (Error).
 *
 * Description:
 * A synthetic example with a schedule dependent deadlock.
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>
#include <unistd.h>

int main(int argc, char** argv)
{
    int rank, size;
    MPI_Status status;
    int buf[2];

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
        buf[0] = 0;
        MPI_Send(&(buf[0]), 1, MPI_INT, 1, 123, MPI_COMM_WORLD);
    }

    if (rank == 1) {
        MPI_Recv(&(buf[0]), 1, MPI_INT, MPI_ANY_SOURCE, 123, MPI_COMM_WORLD, &status);
        printf("Got value %d from process %d\n", buf[0], status.MPI_SOURCE);
        MPI_Recv(&(buf[0]), 1, MPI_INT, 2, 123, MPI_COMM_WORLD, &status);
        printf("Got value %d from process %d\n", buf[0], status.MPI_SOURCE);
    }

    if (rank == 2) {
        buf[0] = 1;
        usleep(1000000);
        MPI_Send(&(buf[0]), 1, MPI_INT, 1, 123, MPI_COMM_WORLD);
    }

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
