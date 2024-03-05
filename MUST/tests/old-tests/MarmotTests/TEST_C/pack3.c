// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_pack3 2>&1 \
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
 *  - MPI_Irecv
 *  - MPI_Pack_size
 *  - MPI_Wait
 *  - MPI_Get_count
 *  - MPI_Unpack
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: pack3.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv)
{
    const int SENDER_RANK = 1;
    const int RECEIVER_RANK = 0;
    const int MSG_TAG = 1;
    const int MAX_SIZE = 100;
    const int COUNT = 1;

    int rank = -1;
    int size = -1;
    int buffersize = 0;
    int* isendbuffer = 0;
    int* irecvbuffer = 0;
    int position = 0;
    int i = 0;
    int i1 = 0;
    int recv_size = 0;

    MPI_Status status;
    MPI_Request mpi_request;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Pack_size(MAX_SIZE, MPI_INT, MPI_COMM_WORLD, &buffersize);
    isendbuffer = malloc(buffersize);
    irecvbuffer = malloc(buffersize);

    /* first we check blocking send and recv for integers */
    if (rank == SENDER_RANK) {
        position = 0;
        for (i = 0; i < MAX_SIZE; i++) {
            MPI_Pack(&i, COUNT, MPI_INT, isendbuffer, buffersize, &position, MPI_COMM_WORLD);
        }
        MPI_Send(isendbuffer, position, MPI_PACKED, RECEIVER_RANK, MSG_TAG, MPI_COMM_WORLD);
    }

    if (rank == RECEIVER_RANK) {
        MPI_Recv(
            irecvbuffer,
            buffersize,
            MPI_PACKED,
            SENDER_RANK,
            MSG_TAG,
            MPI_COMM_WORLD,
            &status);
        MPI_Get_count(&status, MPI_PACKED, &recv_size);
        assert(recv_size <= buffersize);
        assert(recv_size > 0);
        position = 0;
        i = 0;
        while (position < recv_size) {
            MPI_Unpack(irecvbuffer, buffersize, &position, &i1, COUNT, MPI_INT, MPI_COMM_WORLD);
            assert(i1 == i);
            i++;
        }
    }

    /* we introduce a barrier to separate the tests */
    MPI_Barrier(MPI_COMM_WORLD);

    /* second we immediate blocking send and recv for integers */
    if (rank == SENDER_RANK) {
        position = 0;
        for (i = 0; i < MAX_SIZE; i++) {
            MPI_Pack(&i, COUNT, MPI_INT, isendbuffer, buffersize, &position, MPI_COMM_WORLD);
        }
        MPI_Send(isendbuffer, position, MPI_PACKED, RECEIVER_RANK, MSG_TAG, MPI_COMM_WORLD);
    }

    if (rank == RECEIVER_RANK) {
        MPI_Irecv(
            irecvbuffer,
            buffersize,
            MPI_PACKED,
            SENDER_RANK,
            MSG_TAG,
            MPI_COMM_WORLD,
            &mpi_request);
        MPI_Wait(&mpi_request, &status);
        MPI_Get_count(&status, MPI_PACKED, &recv_size);
        assert(recv_size <= buffersize);
        assert(recv_size > 0);
        position = 0;
        i = 0;
        while (position < recv_size) {
            MPI_Unpack(irecvbuffer, buffersize, &position, &i1, COUNT, MPI_INT, MPI_COMM_WORLD);
            assert(i1 == i);
            i++;
        }
    }

    MPI_Finalize();

    return 0;
}
