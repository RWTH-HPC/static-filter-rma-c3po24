// RUN: %must-cc %s -o %must-bin-dir/%basename_t.exe %must-compiler-rma-flags
// RUN: %must-run --must:rma --must:rma-mode %must-rma-mode -np 2 %must-bin-dir/%basename_t.exe 2>&1 | %filecheck -DFILENAME=%basename_t %s

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define PROC_NUM 2
#define WIN_SIZE 1024

int main(int argc, char** argv)
{
    int rank, size;
    MPI_Win win;
    int* win_base;

    MPI_Init(&argc, &argv);
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

    int value1[4] = {1, 1, 1, 1};
    int value2[5] = {1, 2, 3, 4, 5};
    MPI_Win_fence(0, win);

    if (rank == 0) {
        MPI_Accumulate(value2, 5, MPI_INT, 1, 0, 5, MPI_INT, MPI_SUM, win);
        MPI_Accumulate(value1, 4, MPI_INT, 1, 0, 4, MPI_INT, MPI_SUM, win);
    }

    MPI_Win_fence(0, win);
    MPI_Win_free(&win);

    MPI_Finalize();
}

// CHECK-NOT: data race