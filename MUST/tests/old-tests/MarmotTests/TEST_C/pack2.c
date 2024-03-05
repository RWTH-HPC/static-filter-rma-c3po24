// RUN: %must-run %mpiexec-numproc-flag 3 %must-bin-dir/marmot_c_pack2 2>&1 \
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
 *  - MPI_Pack_size
 *  - MPI_Pack
 *  - MPI_Unpack
 *  - MPI_Recv
 *  - MPI_Send
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: pack2.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <mpi.h>
#include <stdio.h>
#include <assert.h>

int main(int argc, char** argv)
{
    const int MSG_TAG = 17;

    int myrank = -1;
    int packsize1 = 0;
    int packsize2 = 0;
    int paket1 = 123456;
    int gep1 = 0;
    int gep2 = 0;
    int empf1 = 0;
    int paket11 = 0;
    int a = 0;
    int b = 0;

    float paket2 = 123.456;
    float empf2 = 0.0;
    float paket22 = 0.0;

    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

    MPI_Pack_size(1, MPI_INT, MPI_COMM_WORLD, &packsize1);
    printf("packsize1: %d \n", packsize1);

    MPI_Pack_size(1, MPI_FLOAT, MPI_COMM_WORLD, &packsize2);
    printf("packsize2: %d \n", packsize2);

    if (myrank == 0) {
        MPI_Pack(&paket1, 1, MPI_INT, &gep1, packsize1, &a, MPI_COMM_WORLD);
        MPI_Send(&gep1, a, MPI_PACKED, 1, MSG_TAG, MPI_COMM_WORLD);
        MPI_Pack(&paket2, 1, MPI_FLOAT, &gep2, packsize2, &b, MPI_COMM_WORLD);
        MPI_Send(&gep2, b, MPI_PACKED, 2, MSG_TAG, MPI_COMM_WORLD);
        printf("paket1=%d, paket2=%f \n", paket1, paket2);
    }

    if (myrank == 1) {
        MPI_Recv(&empf1, packsize1, MPI_PACKED, 0, MSG_TAG, MPI_COMM_WORLD, &status);
        MPI_Unpack(&empf1, packsize1, &a, &paket11, 1, MPI_INT, MPI_COMM_WORLD);
        printf("paket11=%d, \n", paket11);
        /* assert(paket1==paket11); */
    }

    if (myrank == 2) {
        MPI_Recv(&empf2, packsize2, MPI_PACKED, 0, MSG_TAG, MPI_COMM_WORLD, &status);
        MPI_Unpack(&empf2, packsize2, &b, &paket22, 1, MPI_FLOAT, MPI_COMM_WORLD);
        printf("paket22=%f, \n", paket22);
        /* assert(paket2==paket22); */
    }

    MPI_Finalize();

    return 0;
}
