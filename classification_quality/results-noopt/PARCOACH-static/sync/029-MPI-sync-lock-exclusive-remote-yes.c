/* Part of RMARaceBench, under BSD-3-Clause License
 * See https://github.com/RWTH-HPC/RMARaceBench/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RACE LABELS BEGIN
/*
{
    "RACE_KIND": "remote",
    "ACCESS_SET": ["rma write","load"],
    "RACE_PAIR": ["MPI_Put@64","LOAD@77"],
    "NPROCS": 2,
    "CONSISTENCY_CALLS": ["MPI_Win_lock,MPI_Win_unlock"],
    "SYNC_CALLS": ["MPI_Barrier"],
    "DESCRIPTION": "Two conflicting operations put and load, where
non-deterministic lock synchronization leads to a race."
}
*/
// RACE LABELS END

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

#define PROC_NUM 2
#define WIN_SIZE 10

int main(int argc, char **argv) {
  int rank, size;
  MPI_Win win;
  int *win_base;
  int value = 1, value2 = 2;
  int *buf = &value;
  int result;
  int token = 42;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  if (size != PROC_NUM) {
    printf("Wrong number of MPI processes: %d. Expected: %d\n", size, PROC_NUM);
    MPI_Abort(MPI_COMM_WORLD, 1);
  }

  MPI_Win win2;
  int *win_base2;

  MPI_Win_allocate(WIN_SIZE * sizeof(int), sizeof(int), MPI_INFO_NULL,
                   MPI_COMM_WORLD, &win_base, &win);
  MPI_Win_allocate(WIN_SIZE * sizeof(int), sizeof(int), MPI_INFO_NULL,
                   MPI_COMM_WORLD, &win_base2, &win2);

  for (int i = 0; i < WIN_SIZE; i++) {
    win_base[i] = 0;
    win_base2[i] = 0;
  }

  MPI_Barrier(MPI_COMM_WORLD);

  if (rank == 0) {
    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 1, 0, win);
    // CONFLICT
    MPI_Put(&value, 1, MPI_INT, 1, 0, 1, MPI_INT, win);
    MPI_Win_unlock(1, win);

    sleep(1); // do not remove, with that we enforce that rank 1 first locks

    // sync with rank 1
    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 1, 0, win2);
    MPI_Win_unlock(1, win2);
  } else {
    // sync with rank 0
    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 1, 0, win2);
    MPI_Win_unlock(1, win2);
    // CONFLICT
    printf("win_base[0] is %d\n", win_base[0]);
  }

  MPI_Win_free(&win2);

  MPI_Barrier(MPI_COMM_WORLD);
  printf("Process %d: Execution finished, variable contents: value = %d, "
         "value2 = %d, win_base[0] = %d\n",
         rank, *buf, value2, win_base[0]);

  MPI_Win_free(&win);
  MPI_Finalize();

  return 0;
}