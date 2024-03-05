// REQUIRES: tsan

// ALLOW_RETRIES: 3

// RUN: %must-run-tsan %mpiexec-numproc-flag 2 \
// RUN: --must:output-dir %t %must-bin-dir/MpiTSanScatter 2>&1 | %filecheck %check-prefixes %s

// CHECK-DAG: {{.*data race.*MPI_Scatter}}

#include <stdlib.h>
#include <stdio.h>
#include "mpi.h"
#include <omp.h>

#define NUM_THREADS 12

int main(int argc, char* argv[])
{
    int provided;
    int* rbuf = (int*)malloc(NUM_THREADS * sizeof(int));
    int* sbuf = (int*)malloc(NUM_THREADS * sizeof(int));

    MPI_Init_thread(&argc, &argv, MPI_THREAD_FUNNELED, &provided);

    if (provided < MPI_THREAD_FUNNELED) {
        printf("MPI level insufficient\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    int i;
    for (i = 0; i < 12; ++i) {
        sbuf[i] = i;
    }

#pragma omp parallel num_threads(NUM_THREADS)
    {}

#pragma omp parallel num_threads(NUM_THREADS)
    {
        // CHECK-ST-DAG: scatter.c:[[@LINE+1]]
        rbuf[omp_get_thread_num()] = 5;
#pragma omp master
        {
            // CHECK-ST-DAG: scatter.c:[[@LINE+1]]
            MPI_Scatter(
                sbuf,
                NUM_THREADS / 2,
                MPI_INT,
                rbuf,
                NUM_THREADS / 2,
                MPI_INT,
                0,
                MPI_COMM_WORLD);
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
