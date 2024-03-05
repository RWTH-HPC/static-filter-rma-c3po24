/* -*- Mode: C; -*- */
/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Thu Jan 3 2002 */
/* any_src-can-deadlock.c -- deadlock occurs if task 0 receives */
/*                           from task 2 first; the unlikely outcome */
/*                           because we sleep task 2 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#if defined(WIN32)
#include <windows.h>
#else
#include <unistd.h>
#endif

#define buf_size 128

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    int buf0[buf_size];
    int buf1[buf_size];
    MPI_Status status;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(MPI_COMM_WORLD);

    if (nprocs < 3) {
        printf("not enough tasks\n");
    } else if (rank == 0) {
        MPI_Recv(buf1, buf_size, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);

        MPI_Send(buf1, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD);

        MPI_Recv(buf0, buf_size, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);
    } else if (rank == 1) {
        memset(buf0, 0, buf_size);

        MPI_Send(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);

        MPI_Recv(buf1, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
    } else if (rank == 2) {
        memset(buf1, 1, buf_size);

        sleep(5);

        MPI_Send(buf1, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);

    return 0;
}

/* EOF */
