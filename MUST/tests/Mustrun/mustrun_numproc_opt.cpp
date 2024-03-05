/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file mustrun_numproc_opt.c
 * This is a test for the mustrun script.
 *
 * Description:
 * This test checks if mustrun preserves the -n or -np option after the first "--" marker.
 * See: https://git-ce.rwth-aachen.de/hpc-research/correctness/MUST/-/issues/37
 *
 *  @date 09.09.2022
 *  @author Joachim Protze
 */

// RUN: %must-run %must-bin-dir/mustrun_numproc_opt -d 1 -n 1 | %filecheck --check-prefix=CHECK-N %s
// RUN: %must-run %must-bin-dir/mustrun_numproc_opt -d 1 -np 1 | %filecheck --check-prefix=CHECK-NP %s

// RUN: %must-run -np 1 -- %must-bin-dir/mustrun_numproc_opt -d 1 -n 2 | %filecheck --check-prefix=CHECK-N-MARKED %s
// RUN: %must-run -n 1 -- %must-bin-dir/mustrun_numproc_opt -d 1 -np 2 | %filecheck --check-prefix=CHECK-NP-MARKED %s

// RUN: %must-run -np 1 -- %must-bin-dir/mustrun_numproc_opt -d 1 -- -n 2 | %filecheck --check-prefix=CHECK-N-MARKED-TWICE %s
// RUN: %must-run -n 1 -- %must-bin-dir/mustrun_numproc_opt -d 1 -- -np 2 | %filecheck --check-prefix=CHECK-NP-MARKED-TWICE %s

// CHECK-N-NOT: -n 1
// CHECK-N: -d 1
// CHECK-NP-NOT: -np 1
// CHECK-NP: -d 1

// CHECK-N-MARKED-NOT: -n 1
// CHECK-N-MARKED: -d 1 -n 2
// CHECK-NP-MARKED-NOT: -np 1
// CHECK-NP-MARKED: -d 1 -np 2

// CHECK-N-MARKED-TWICE-NOT: -n 1
// CHECK-N-MARKED-TWICE: -d 1 -- -n 2
// CHECK-NP-MARKED-TWICE-NOT: -np 1
// CHECK-NP-MARKED-TWICE: -d 1 -- -np 2

// echo "Received options: $@"

#include <stdio.h>
#include <string>
#include "mpi.h"
int main(int argc, char** argv)
{
    // The mpi calls are only here to prevent possible confusion of must. It seems
    // to timeout if called with the unusual case of only one mpi process.
    int rank = -1, i;

    if (argc > 1) {
        std::string output = "Received options: ";
        for (i = 1; i < argc; i++)
            output += std::string(argv[i]) + " ";
        printf("%s\n", output.c_str());
        fflush(stdout);
    }

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Finalize();
    return 1;
}
