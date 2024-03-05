// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_pack4 2>&1 \
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
 *  - MPI_Irecv
 *  - MPI_Wait
 *  - MPI_Get_count
 *
 *  Communication is done between node 0 and node 1. 
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: pack4.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv)
{
    const int MSG_TAG = 1;
    const int MAX_SIZE = 100;
    const int LEFT = 0;
    const int RIGHT = 1;

    int rank = -1;
    int size = -1;
    int buffersize = 0;
    int* isendbuffer[2] = {0, 0};
    int* irecvbuffer[2] = {0, 0};
    int position[2] = {0, 0};
    int i = 0;
    int i1 = 0;
    int dir = 0;
    int recv_size = 0;
    int left_neighbour = -1;
    int right_neighbour = -1;

    MPI_Status status[4];
    MPI_Request mpi_request[4];

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    /* calculate left and right neighbours with periodic boundaries: */
    left_neighbour = (rank + size - 1) % size;
    right_neighbour = (rank + 1) % size;

    /* printf("left_neighbour= %d \n right_neighbour= %d \n", 
       left_neighbour,right_neighbour); 
     */

    /* we allocate the buffers for communication: */
    MPI_Pack_size(MAX_SIZE, MPI_INT, MPI_COMM_WORLD, &buffersize);
    isendbuffer[LEFT] = malloc(buffersize);
    isendbuffer[RIGHT] = malloc(buffersize);
    irecvbuffer[LEFT] = malloc(buffersize);
    irecvbuffer[RIGHT] = malloc(buffersize);

    /* printf("buffersize= %d \n", buffersize); */

    /* pack the data into the sendbuffers: */
    position[LEFT] = 0;
    position[RIGHT] = 0;

    for (i = 0; i < MAX_SIZE; i++) {
        MPI_Pack(&i, 1, MPI_INT, isendbuffer[LEFT], buffersize, &position[LEFT], MPI_COMM_WORLD);
        MPI_Pack(&i, 1, MPI_INT, isendbuffer[RIGHT], buffersize, &position[RIGHT], MPI_COMM_WORLD);
    }

    /* start the send and receive requests: */
    MPI_Isend(
        isendbuffer[LEFT],
        position[LEFT],
        MPI_PACKED,
        left_neighbour,
        MSG_TAG,
        MPI_COMM_WORLD,
        &mpi_request[LEFT]);
    MPI_Isend(
        isendbuffer[RIGHT],
        position[RIGHT],
        MPI_PACKED,
        right_neighbour,
        MSG_TAG,
        MPI_COMM_WORLD,
        &mpi_request[RIGHT]);
    MPI_Irecv(
        irecvbuffer[LEFT],
        buffersize,
        MPI_PACKED,
        left_neighbour,
        MSG_TAG,
        MPI_COMM_WORLD,
        &mpi_request[LEFT + 2]);
    MPI_Irecv(
        irecvbuffer[RIGHT],
        buffersize,
        MPI_PACKED,
        right_neighbour,
        MSG_TAG,
        MPI_COMM_WORLD,
        &mpi_request[RIGHT + 2]);
    MPI_Waitall(4, mpi_request, status);

    /* printf("status[3].source=%d \n",status[3].MPI_SOURCE); */
    /* printf("status[2].source=%d \n",status[2].MPI_SOURCE); */

    for (dir = LEFT; dir <= RIGHT; dir++) {
        MPI_Get_count(&status[dir + 2], MPI_PACKED, &recv_size);
        /* printf("rank:%d,  Get_count: recv_size= %d \n",rank,recv_size); */

        assert(recv_size <= buffersize);
        assert(recv_size > 0);
        position[dir] = 0;
        i = 0;
        while (position[dir] < recv_size) {
            MPI_Unpack(
                irecvbuffer[dir],
                buffersize,
                &position[dir],
                &i1,
                1,
                MPI_INT,
                MPI_COMM_WORLD);
            /*      printf(" %d == %d \n",i,i1); */
            assert(i1 == i);
            i++;
        }
    }

    MPI_Finalize();

    return 0;
}
