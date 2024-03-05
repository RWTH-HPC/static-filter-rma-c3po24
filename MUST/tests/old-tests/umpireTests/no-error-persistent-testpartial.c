/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/umpire_no-error-persistent-testpartial 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/* Creator: Bronis de Supinski (bronis@llnl.gov) */
/* no-error-testpartial.c -- persistent requests and testany/testsome */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>
#include <assert.h>

#define BUF_SIZE 128

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    MPI_Comm comm = MPI_COMM_WORLD;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    int buf[BUF_SIZE * 2];
    int i, j, k, index, outcount, flag;
    int indices[2];
    MPI_Request aReq[2];
    MPI_Status aStatus[2];

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    if (rank == 0) {
        /* set up persistent sends... */
        MPI_Send_init(&buf[0], BUF_SIZE, MPI_INT, 1, 0, comm, &aReq[0]);
        MPI_Send_init(&buf[BUF_SIZE], BUF_SIZE, MPI_INT, 1, 1, comm, &aReq[1]);

        /* initialize the send buffers */
        for (i = 0; i < BUF_SIZE; i++) {
            buf[i] = i;
            buf[BUF_SIZE + i] = BUF_SIZE - 1 - i;
        }
    }

    for (k = 0; k < 4; k++) {
        if (rank == 1) {
            /* zero out the receive buffers */
            bzero(buf, sizeof(int) * BUF_SIZE * 2);
        }

        MPI_Barrier(MPI_COMM_WORLD);

        if (rank == 0) {
            /* start the persistent sends... */
            if (k % 2) {
                MPI_Startall(2, &aReq[0]);
            } else {
                for (j = 0; j < 2; j++) {
                    MPI_Start(&aReq[j]);
                }
            }

            /* complete the sends */
            if (k < 2) {
                /* use MPI_Testany */
                for (j = 0; j < 2; j++) {
                    flag = 0;
                    while (!flag) {
                        MPI_Testany(2, aReq, &index, &flag, aStatus);
                    }
                }
            } else {
                /* use MPI_Testsome */
                j = 0;
                while (j < 2) {
                    outcount = 0;
                    while (!outcount) {
                        MPI_Testsome(2, aReq, &outcount, indices, aStatus);
                    }
                    j += outcount;
                }
            }
        } else if (rank == 1) {
            /* set up receives for all of the sends */
            for (j = 0; j < 2; j++) {
                MPI_Irecv(&buf[j * BUF_SIZE], BUF_SIZE, MPI_INT, 0, j, comm, &aReq[j]);
            }
            /* complete all of the receives... */
            MPI_Waitall(2, aReq, aStatus);
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0) {
        /* free the persistent requests */
        for (i = 0; i < 2; i++) {
            MPI_Request_free(&aReq[i]);
        }
    }

    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */
