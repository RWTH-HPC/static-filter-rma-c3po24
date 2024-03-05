/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 1 %must-bin-dir/umpire_type-no-free2 \
// RUN: 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are .* datatypes that are not freed when MPI_Finalize was issued}}

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) Tue Dec 10 2002 */

/* type-no-free2.c -- create many types, failing to free some */

/* NOTE: Some value of ITERATIONS will imply resource exhaustion */
/*       either in Umpire or MPI no matter how things are implemented */
/*       the best we can hope for is to fail gracefully... */
/* 10000 breaks umpire due to running out of memory as of 12/12/02... */
/* FAILURE IS NOT GRACEFUL AS OF THIS TIME... */
#define ITERATIONS 10
#define TYPES_PER_ITERATION 3
#define TYPES_LOST_PER_ITERATION 1
#define TYPES_TO_COMMIT 1

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
    MPI_Datatype newtype[TYPES_PER_ITERATION];

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(MPI_COMM_WORLD);

    for (i = 0; i < ITERATIONS; i++) {
        for (j = 0; j < TYPES_PER_ITERATION; j++) {
            MPI_Type_contiguous(128, MPI_INT, &newtype[j]);

            if (j >= TYPES_PER_ITERATION - TYPES_TO_COMMIT) {
                MPI_Type_commit(&newtype[j]);
            }

            if (j < TYPES_PER_ITERATION - TYPES_LOST_PER_ITERATION) {
                MPI_Type_free(&newtype[j]);
            }
        }

        if (((i % (ITERATIONS / 10)) == 0) && (rank == 0))
            printf("iteration %d completed\n", i);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    printf("(%d) Finished normally\n", rank);
    MPI_Finalize();
    return 0;
}

/* EOF */
