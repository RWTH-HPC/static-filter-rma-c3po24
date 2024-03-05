
#include <stdio.h>
#include <mpi.h>
#include <stddef.h>

#define NUM_TYPES 8

int main(int argc, char** argv)
{
    MPI_Datatype tNew;
    int blocklens[NUM_TYPES] = {1, 1, 1, 1, 1, 1, 1, 1};
    MPI_Aint displs[NUM_TYPES] = {0, 64, 128, 192, 256, 320, 384, 448};
    MPI_Datatype types[NUM_TYPES] = {
        MPI_INTEGER,
        MPI_DOUBLE_COMPLEX,
        MPI_REAL8,
        MPI_INTEGER8,
        MPI_COMPLEX8,
        MPI_LOGICAL8,
        MPI_2REAL,
        MPI_2DOUBLE_COMPLEX};

    MPI_Init(&argc, &argv);

    MPI_Type_struct(NUM_TYPES, blocklens, displs, types, &tNew);

    MPI_Type_free(&tNew);

    MPI_Finalize();

    return 0;
}
