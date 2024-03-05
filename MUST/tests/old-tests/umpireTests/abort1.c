/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/umpire_abort1 2>&1 \
// RUN: | %filecheck --implicit-check-not '[MUST-REPORT]{{.*(Error: from: \
// RUN: call MPI_Abort@.*0: Argument 2 [(]errorcode[)] has to be a \
// RUN: non-negative integer, but is negative)}}' %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Abort@.*[1-9]: Argument 2 [(]errorcode[)] has to be a non-negative integer, but is negative}}

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Tue Oct 29 2002 */
/* abort1.c -- call MPI abort in all tasks other than zero... */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;

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
    } else {
        if (rank) {
            printf("(%d) Aborting\n", rank);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();
    printf("(%d) Finished normally\n", rank);
    return 0;
}

/* EOF */
