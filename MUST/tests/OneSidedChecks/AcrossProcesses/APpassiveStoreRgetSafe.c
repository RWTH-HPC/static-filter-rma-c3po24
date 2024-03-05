// RUN: %must-cc %s -o %must-bin-dir/%basename_t.exe %must-compiler-rma-flags
// RUN: %must-run --must:rma --must:rma-mode %must-rma-mode -np 2 %must-bin-dir/%basename_t.exe 2>&1 | %filecheck -DFILENAME=%basename_t %s

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char** argv)
{
    int rank = 0;
    int size = 0;
    int win_buf_size = 1000;
    float* win_buf;
    float* buf_recv;
    MPI_Win win;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Win_allocate(
        (MPI_Aint)((win_buf_size) * sizeof(float)),
        sizeof(float),
        MPI_INFO_NULL,
        MPI_COMM_WORLD,
        &win_buf,
        &win);
    MPI_Win_lock_all(0, win);

    if (rank == 1) {
        for (int i = 0; i < win_buf_size; i++)
            win_buf[i] = rank;
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0) {
        buf_recv = (float*)malloc(win_buf_size * sizeof(float));
        MPI_Request req;
        MPI_Rget(buf_recv, win_buf_size, MPI_FLOAT, 1, 0, win_buf_size, MPI_FLOAT, win, &req);

        // origin completion of get (reading operation) implies target completion
        MPI_Wait(&req, MPI_STATUS_IGNORE);

        free(buf_recv);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 1) {
        win_buf[0] = 42;
    }

    MPI_Win_unlock_all(win);
    MPI_Win_free(&win);
    MPI_Finalize();
}

// CHECK-NOT: data race