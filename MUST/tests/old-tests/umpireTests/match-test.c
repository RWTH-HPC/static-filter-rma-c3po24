/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 3 %must-bin-dir/umpire_match-test \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*The application issued a set of MPI calls that can cause a deadlock.*MPI_Send}}

/* Creator: Tobias Hilbrich */

/* match-test.c -- post some calls that match or do not match 
 *                 Intended to test and debug message matching.
 */

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
    int buf[buf_size];
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    MPI_Status status;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    switch (rank) {
    case 0:
        MPI_Send(buf, buf_size, MPI_INT, 2, 123, MPI_COMM_WORLD);
        MPI_Barrier(MPI_COMM_WORLD);

        MPI_Send(buf, buf_size, MPI_INT, 1, 333, MPI_COMM_WORLD);
        break;

    case 1:
        MPI_Recv(buf, buf_size, MPI_INT, 2, 222, MPI_COMM_WORLD, &status);
        MPI_Barrier(MPI_COMM_WORLD);
        sleep(5);
        MPI_Recv(buf, buf_size, MPI_INT, MPI_ANY_SOURCE, 333, MPI_COMM_WORLD, &status);
        break;

    case 2:
        MPI_Recv(buf, buf_size, MPI_INT, 0, 123, MPI_COMM_WORLD, &status);
        MPI_Send(buf, buf_size, MPI_INT, 1, 222, MPI_COMM_WORLD);
        MPI_Barrier(MPI_COMM_WORLD);

        MPI_Send(buf, buf_size, MPI_INT, 1, 333, MPI_COMM_WORLD);
        break;

    default:
        /* additional ranks that actually don't want ... */
        MPI_Barrier(MPI_COMM_WORLD); /* We also have to participate in the Barrier ... */
        break;
    }

    printf("(%d) Finished normally\n", rank);
    MPI_Finalize();
    return 0;
}

/* EOF */
