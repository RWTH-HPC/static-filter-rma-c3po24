// RUN: %must-run %mpiexec-numproc-flag 3 \
// RUN: %must-bin-dir/marmot_c_irreproduce1 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

/**
 *  @file
 *
 *  This program is a very simple MPI program to verify that the 
 *  library works correctly.
 *  
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Finalize 
 *  - MPI_Comm_rank
 *  - MPI_Comm_size
 *  - MPI_Send
 *  - MPI_Recv
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: irreproduce1.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include "mpi.h"

const int COUNT = 100;
const int SEND_RECV_COUNT = 1;
const int MSG_TAG = 17;

void consume_and_print(int size)
{
    int i = 0;
    int nr = 0;

    MPI_Status status;

    for (i = 0; i < COUNT * (size - 1); i++) {
        MPI_Recv(
            &nr,
            SEND_RECV_COUNT,
            MPI_INT,
            MPI_ANY_SOURCE,
            MPI_ANY_TAG,
            MPI_COMM_WORLD,
            &status);
        printf("%d", nr);
        if (((i + 1) % 40) == 0) {
            printf("\n");
        }
    }
    printf("\n");
} /* end consume_and_print */

void produce(int rank, int size)
{
    int i;
    int nr;
    nr = rank;
    for (i = 0; i < COUNT; i++) {
        MPI_Send(&nr, SEND_RECV_COUNT, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD);
    }
} /* end produce */

int main(int argc, char** argv)
{
    int rank = -1;
    int size = -1;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 3) {
        fprintf(stderr, " This program needs at least 3 PEs!\n");
    } else {
        if (rank == 0) {
            consume_and_print(size);
            /*MPI_Barrier(MPI_COMM_WORLD); */
            /*consume_and_print(size); */
        } else {
            produce(rank, size);
            /*MPI_Barrier(MPI_COMM_WORLD); */
            /*produce(rank,size); */
        }
    }

    MPI_Finalize();

    return 0;
} /* end main */
