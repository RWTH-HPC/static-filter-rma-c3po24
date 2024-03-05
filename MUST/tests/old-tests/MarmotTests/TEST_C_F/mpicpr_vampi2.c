#include <stdio.h>
#include <assert.h>
#include "mpi.h"
#ifdef MARMOT_VAMPI
#include "replacempi.h"
#include "marmot.h"
#endif

void mpicpr_(void)
{
    const int MSG_TAG = 17;
    const int COUNT = 1;

    int size = -1;
    int rank = -1;
    int value = -1;

    MPI_Status status;

    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (rank == 0) {
        /* going to receive message */
        MPI_Recv(&value, COUNT, MPI_INTEGER, 1, MSG_TAG, MPI_COMM_WORLD, &status);
        assert(value == 19);
    }

    if (rank == 1) {
        /* going to send message */
        value = 19;
        MPI_Send(&value, COUNT, MPI_INT, 0, MSG_TAG, MPI_COMM_WORLD);
    }

    printf(" I am rank %d of %d PEs\n", rank, size);

    return;
}
