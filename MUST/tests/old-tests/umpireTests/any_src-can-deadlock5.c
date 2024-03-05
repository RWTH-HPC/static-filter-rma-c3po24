/* -*- Mode: C; -*- */
// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 3 \
// RUN: %must-bin-dir/umpire_any_src-can-deadlock5 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-RUNTIME] ERROR: MUST detected a deadlock, detailed information is available in the MUST output file.

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Tue Aug 26 2003 */
/* any_src-can-deadlock5.c -- deadlock occurs if task 0 receives */
/*                            from task 1 first; sleeps generally */
/*                            make order 1 before 2 with all task */
/*                            0 ops being posted after both 1 and 2 */

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
    MPI_Request req;

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
        sleep(10);

        MPI_Irecv(buf0, buf_size, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &req);

        MPI_Recv(buf1, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD, &status);

        MPI_Send(buf1, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD);

        MPI_Recv(buf1, buf_size, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);

        MPI_Wait(&req, &status);
    } else if (rank == 1) {
        memset(buf0, 0, buf_size);

        MPI_Send(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);

        MPI_Recv(buf1, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);

        MPI_Send(buf1, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);
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
