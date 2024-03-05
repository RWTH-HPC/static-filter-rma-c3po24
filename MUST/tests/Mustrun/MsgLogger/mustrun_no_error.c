/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run-errorcode --must:output html %mpiexec-numproc-flag 2 %must-bin-dir/mustrun_msglogger_no_error
// RUN: %must-run-errorcode --must:output stdout %mpiexec-numproc-flag 2 %must-bin-dir/mustrun_msglogger_no_error
// RUN: %must-run-errorcode --must:output json %mpiexec-numproc-flag 2 %must-bin-dir/mustrun_msglogger_no_error

/**
 * @file mustrun_no_error.c
 * No error dummy to test return code of different output modes.
 *
 *  @date 09.08.2023
 *  @author Felix Tomski
 */

#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv)
{
    int rank;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    printf("Signing off, rank %d\n", rank);

    MPI_Finalize();

    return 0;
}
