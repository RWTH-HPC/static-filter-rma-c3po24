/* -*- Mode: C; -*- */
// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/umpire_waitany-deadlock 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-RUNTIME] ERROR: MUST detected a deadlock, detailed information is available in the MUST output file.

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Fri Mar  17 2000 */
/* waitany-deadlock.c -- deadlock involving MPI_Waitany call */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>
#include <assert.h>

#define buf_size 128

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    int buf0[buf_size];
    int buf1[buf_size];
    int buf2[buf_size];
    int i, flipbit, done;
    MPI_Status status;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(MPI_COMM_WORLD);

    if (nprocs < 2) {
        printf("not enough tasks\n");
    } else if (rank == 0) {
        MPI_Request reqs[3];

        MPI_Irecv(buf0, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD, &reqs[0]);
        MPI_Irecv(buf1, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD, &reqs[1]);
        MPI_Irecv(buf2, buf_size, MPI_INT, 1, 0, MPI_COMM_WORLD, &reqs[2]);

        for (i = 3; i > 0; i--) {
            MPI_Waitany(i, reqs, &done, &status);

            assert(done == (i - 1));

            /* don't let next one start until after waitany call... */
            MPI_Send(&flipbit, 1, MPI_INT, 1, i, MPI_COMM_WORLD);
        }
    } else if (rank == 1) {
        memset(buf0, 1, buf_size);

        for (i = 3; i > 0; i--) {
            MPI_Recv(&flipbit, 1, MPI_INT, 0, i, MPI_COMM_WORLD, &status);

            MPI_Send(buf0, buf_size, MPI_INT, 0, 0, MPI_COMM_WORLD);
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */
