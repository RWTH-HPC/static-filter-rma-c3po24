/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file mustrun_passes_correct_envvars.c
 * This is a test for the mustrun script.
 *
 * Description:
 * Checks if TSAN_OPTIONS are parsed correctly.
 *
 *  @date 07.04.2023
 *  @author Simon Schwitanski
 */

// RUN: env TSAN_OPTIONS="report_bugs=0 exitcode=42 log_path='my complicated logpath'" \
// RUN: %must-run %mpiexec-numproc-flag 1 \
// RUN: %must-bin-dir/mustrun_tsan_options | %filecheck %s

// CHECK-DAG: report_bugs=0
// CHECK-DAG: exitcode=42
// CHECK-DAG: log_path='my complicated logpath'
// CHECK-NOT: ERROR

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv)
{
    // The mpi calls are only here to prevent possible confusion of must. It seems
    // to timeout if called with the unusual case of only one mpi process.
    int rank = -1;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    const char* const value = getenv("TSAN_OPTIONS");
    // Test if the variable exists
    if (value == NULL) {
        printf("ERROR: the environment variable TSAN_OPTIONS is not set");
        MPI_Finalize();
        return 1;
    }

    // Print out contents for sanity checks
    printf("TSAN_OPTIONS: %s\n", value);

    MPI_Finalize();

    return 0;
}
