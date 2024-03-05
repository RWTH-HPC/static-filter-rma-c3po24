// RUN: %must-cc %s -o %must-bin-dir/%basename_t.exe %must-compiler-rma-flags
// RUN: %must-run --must:rma --must:rma-mode %must-rma-mode -np 2 %must-bin-dir/%basename_t.exe 2>&1 | %filecheck -DFILENAME=%basename_t %s

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

#define PROC_NUM 2
#define WIN_SIZE 1024

int main(int argc, char** argv)
{
    int rank, size;
    MPI_Win win;
    int* win_base;

    int required = MPI_THREAD_MULTIPLE, provided;
    MPI_Init_thread(&argc, &argv, required, &provided);

    if (provided < required) {
        printf("ERROR");
        MPI_Abort(MPI_COMM_WORLD, 0);
    }
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size != PROC_NUM) {
        printf("Wrong number of MPI processes. Expected: %d\n", PROC_NUM);
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Win_allocate(
        WIN_SIZE * sizeof(int),
        sizeof(int),
        MPI_INFO_NULL,
        MPI_COMM_WORLD,
        &win_base,
        &win);
    for (int i = 0; i < WIN_SIZE; i++) {
        win_base[i] = 0;
    }
    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0) {
        int value = 42;
        MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 1, 0, win);
        MPI_Request req;
        // CHECK-DAG: [[FILENAME]]:[[@LINE+1]]
        MPI_Rget(&value, 1, MPI_INT, 1, 0, 1, MPI_INT, win, &req);
        // CHECK-DAG: [[FILENAME]]:[[@LINE+1]]
        printf("PRINT VALUE %d", value);
        MPI_Wait(&req, MPI_STATUS_IGNORE);
        MPI_Win_unlock(1, win);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 1) {
        printf("received value = %d\n", win_base[0]);
    }

    MPI_Win_free(&win);
    MPI_Finalize();
}

// CHECK-DAG: data race