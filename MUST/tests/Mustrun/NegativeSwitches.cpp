/* Part of the MUST Project, under BSD-3-Clause License
* See https://hpc.rwth-aachen.de/must/LICENSE for license information.
* SPDX-License-Identifier: BSD-3-Clause
*/

/* This test checks if mustrun parses layouts passed via --must:layout correctly. */

// RUN: env MUST_DISTRIBUTED=1 %must-run -np 4 --must:no-distributed --must:info | %filecheck --check-prefix=CHECK-NO-FLAG %s
// CHECK-NO-FLAG: centralized checks with fall-back application crash handling (very slow)

// RUN: env MUST_DISTRIBUTED=1 %must-run -np 4 --must:info | %filecheck --check-prefix=CHECK-WITHOUT-NO-FLAG %s
// CHECK-WITHOUT-NO-FLAG: distributed checks without application crash handling

// RUN: env MUST_LAYOUT=layout.xml %must-run -np 4 --must:no-layout %must-bin-dir/NegativeSwitches | %filecheck --check-prefix=CHECK-NO-ARG %s
// CHECK-NO-ARG-NOT: with given layout.xml

#include <stdio.h>
#include <string>
#include "mpi.h"

int main(int argc, char** argv)
{
    // The mpi calls are only here to prevent possible confusion of must. It seems
    // to timeout if called with the unusual case of only one mpi process.
    int rank = -1;
    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Finalize();
    return 1;
}
