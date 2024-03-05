/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 5 %must-bin-dir/collGatherEx2NoError \
// RUN: 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

// RUN: %must-run %mpiexec-numproc-flag 5 --must:layout \
// RUN: %builddir/tests/DeadlockDetection/DCollectiveMatch/collGatherEx2NoErrorlayout.xml \
// RUN: %must-bin-dir/DcollGatherEx2NoError 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

/**
 * @file collGatherEx1NoError.c
 * A test with a correct MPI_Gather call (No Error).
 *
 * Description:
 * All processes execute an MPI_Gather with matching and valid arguments.
 * The receiver side uses a type of 2 consecutive MPI_INT while the sender sends two single MPI_INT. (No Error)
 *
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
    int rank, size, *temp = NULL, sbuf[2];
    MPI_Datatype rType = MPI_DATATYPE_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    //Enough tasks ?
    if (size < 2) {
        printf("This test needs at least 2 processes!\n");
        MPI_Finalize();
        return 1;
    }

    //Say hello
    printf("Hello, I am rank %d of %d processes.\n", rank, size);
    sbuf[0] = rank;
    sbuf[1] = size;

    if (rank == 0) {
        temp = (int*)malloc(sizeof(int) * size * 2);
        MPI_Type_contiguous(2, MPI_INT, &rType);
        MPI_Type_commit(&rType);
    }

    MPI_Gather(sbuf, 2, MPI_INT, temp, 1, rType, 0, MPI_COMM_WORLD);

    if (rank == 0) {
        MPI_Type_free(&rType);
        for (int i = 0; i < size; i++)
            printf("Rank 0 received temp[%i]=%i\n", i * 2, temp[i * 2]);
        if (temp)
            free(temp);
    }

    //Say bye bye
    printf("Signing off, rank %d.\n", rank);

    MPI_Finalize();

    return 0;
}
