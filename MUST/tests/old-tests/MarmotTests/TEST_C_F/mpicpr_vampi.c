#include <stdio.h>
#include <assert.h>
#include "mpi.h"
#ifdef MARMOT_VAMPI
#include "replacempi.h"
#include "marmot.h"
#endif

void mpicpr_(void)
{
    int size = -1;
    int rank = -1;

    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    printf(" I am rank %d of %d PEs\n", rank, size);

    return;
}
