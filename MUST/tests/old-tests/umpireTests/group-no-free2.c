/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/umpire_group-no-free2 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*There are 1 groups that are not freed when MPI_Finalize was issued}}

/* Creator: Bronis R. de Supinski (bronis@llnl.gov) */

/* group-no-free2.c -- construct many groups without freeing some */

/* NOTE: Some value of ITERATIONS will imply resource exhaustion */
/*       either in Umpire or MPI no matter how things are implemented */
/*       the best we can hope for is to fail gracefully... */
/* UNKNOWN N breaks umpire due to running out of memory as of 12/20/02... */
/* FAILURE IS NOT GRACEFUL AS OF THIS TIME... */
#define ITERATIONS 100
#define GROUPS_PER_ITERATION 3
#define GROUPS_LOST_PER_ITERATION 1

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
    MPI_Group newgroup[GROUPS_PER_ITERATION];

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(MPI_COMM_WORLD);

    for (i = 0; i < ITERATIONS; i++) {
        for (j = 0; j < GROUPS_PER_ITERATION; j++) {
            MPI_Comm_group(MPI_COMM_WORLD, &newgroup[j]);

            if (j < GROUPS_PER_ITERATION - GROUPS_LOST_PER_ITERATION) {
                MPI_Group_free(&newgroup[j]);
            }
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);
    printf("(%d) Finished normally\n", rank);
    MPI_Finalize();
    return 0;
}

/* EOF */
