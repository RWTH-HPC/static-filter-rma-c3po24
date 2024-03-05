/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// REQUIRES: HAVE_MPI_TYPE_CREATE_RESIZED
// RUN: %must-run %mpiexec-numproc-flag 1 \
// RUN: %must-bin-dir/resizeExpandByStructNoLimits 2>&1 \
// RUN: | %filecheck --implicit-check-not '[MUST-REPORT]{{.*(Error|ERROR)}}' %s

/**
 * @file resizeExpandByStructNolimit.c
 * A datatype implementation test.
 * Contains no errors.
 *
 * @author Joachim Protze
 */

#include "datatypetest.h"

int main(int argc, char** argv)
{

    MPI_Init(&argc, &argv);

    MPI_Datatype contitype, resizedtype;

    MPI_Type_contiguous(5, MPI_INT, &contitype);
    mpiResizedByStruct(contitype, -4, 28, &resizedtype);

    // call the real test - this is a macro, that directs to the specific test
    datatypeTest(resizedtype);

    MPI_Type_free(&resizedtype);
    MPI_Type_free(&contitype);

    MPI_Finalize();

    return 0;
}
