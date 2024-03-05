/* -*- Mode: C; -*- */
// XFAIL: *
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/umpire_lost-request3 \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*The memory regions to be transfered by this receive operation overlap.*There are 1000 requests that are not freed when MPI_Finalize was issued}}

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Fri Dec 20 2002 */
/* lost-request3.c -- lose lots of requests */

/* NOTE: Some value of ITERATIONS will imply resource exhaustion */
/*       either in Umpire or MPI no matter how things are implemented */
/*       the best we can hope for is to fail gracefully... */
/* UNKNOWN N breaks umpire due to running out of memory as of 12/20/02... */
/* FAILURE IS NOT GRACEFUL AS OF THIS TIME... */
#define ITERATIONS 1000
#define REQS_PER_ITERATION 3
#define REQS_LOST_PER_ITERATION 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#define buf_size 128

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    int i, j;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    int buf[buf_size * REQS_PER_ITERATION];
    MPI_Request req[REQS_PER_ITERATION];
    MPI_Status statuses[REQS_PER_ITERATION];

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(MPI_COMM_WORLD);

    /* 0 sends 1 two messages, but the request gets overwritten */
    switch (rank) {
    case 0:
        for (i = 0; i < ITERATIONS; i++) {
            memset(buf, 1, buf_size * REQS_PER_ITERATION);

            for (j = 0; j < REQS_PER_ITERATION; j++) {
                MPI_Isend(&buf[j * buf_size], buf_size, MPI_INT, 1, j, MPI_COMM_WORLD, &req[j]);
            }

            MPI_Waitall(REQS_PER_ITERATION, req, statuses);
        }

        break;

    case 1:
        for (i = 0; i < ITERATIONS; i++) {
            memset(buf, 2, buf_size * REQS_PER_ITERATION);

            for (j = 0; j < REQS_PER_ITERATION; j++) {
                MPI_Irecv(&buf[j * buf_size], buf_size, MPI_INT, 0, j, MPI_COMM_WORLD, &req[j]);
            }

            /* do some work here and get confused */
            MPI_Waitall(REQS_PER_ITERATION - REQS_LOST_PER_ITERATION, req, statuses);
        }

        break;

    default:
        /* do nothing */
        break;
    }

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */
