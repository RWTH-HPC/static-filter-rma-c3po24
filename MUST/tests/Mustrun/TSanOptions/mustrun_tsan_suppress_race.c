/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file mustrun_tsan_suppress_race.c
 * This is a test for the mustrun script.
 * 
 *
 * Description:
 * Test case that checks whether mustrun respects user supppression files.
 * Without the user suppressions file, the race should be reported.
 * With the user suppressions file, the race should *not* be reported.
 *
 *  @date 07.04.2023
 *  @author Simon Schwitanski
 */

// REQUIRES: tsan
// ALLOW_RETRIES: 3

// Case 1: Run without user suppressions file, expect TSan race report
// RUN: env TSAN_OPTIONS="log_path=stdout" %must-run %mpiexec-numproc-flag 1 \
// RUN: %must-bin-dir/mustrun_tsan_suppress_race | %filecheck --check-prefix=CHECK %s

// CHECK: data race

// Case 2: Run with user suppressions file, expect no race report
// RUN: env TSAN_OPTIONS="log_path=stdout suppressions=%builddir/tests/Mustrun/TSanOptions/mytsan_suppressions.txt" \
// RUN: %must-run %mpiexec-numproc-flag 1 \
// RUN: %must-bin-dir/mustrun_tsan_suppress_race | %filecheck --check-prefix=CHECK-SUPPRESS %s

// CHECK-SUPPRESS-NOT: data race

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv)
{
    // The mpi calls are only here to prevent possible confusion of must. It seems
    // to timeout if called with the unusual case of only one mpi process.
    int rank = -1;
    int provided;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_SINGLE, &provided);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    int var = 0;

#pragma omp parallel num_threads(8) shared(var)
    {
        var++;
    }

    int error = (var != 2);
    printf("error = %d", error);

    MPI_Finalize();

    return 0;
}
