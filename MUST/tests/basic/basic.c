/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 4 --must:layout \
// RUN: %builddir/tests/basic/layout.xml --must:analyses \
// RUN: %builddir/tests/basic/analysis_spec.xml %must-bin-dir/basic 2>&1 \
// RUN: | %filecheck %s --check-prefixes=CHECK,CHECK-LOG

// RUN: %must-run --must:layout \
// RUN: %builddir/tests/basic/layout.xml --must:analyses \
// RUN: %builddir/tests/basic/analysis_spec.xml %must-bin-dir/basic 2>&1 \
// RUN: | %filecheck %s --check-prefixes=CHECK,CHECK-LOG

// RUN: %must-run %mpiexec-numproc-flag 4 --must:nodesize 2 \
// RUN: --must:distributed %must-bin-dir/basic 2>&1 --must:mpimode SMPD | \
// RUN: %filecheck %s

// CHECK-DAG: Hello, I am 0 of 4 tasks with pid
// CHECK-DAG: Hello, I am 1 of 4 tasks with pid
// CHECK-DAG: Hello, I am 2 of 4 tasks with pid
// CHECK-DAG: Hello, I am 3 of 4 tasks with pid

// CHECK-LOG-DAG: [MUST-REPORT]{{.*Test log message!}}
// CHECK-LOG-DAG: [MUST-REPORT]{{.*MPI_Init.*rank}}

/**
 * @file hello.c
 * A must hello world test.
 * Contains no errors.
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char** argv)
{
    int rank, size;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf("Hello, I am %d of %d tasks with pid %i.\n", rank, size, getpid());

    MPI_Finalize();

    return 0;
}
