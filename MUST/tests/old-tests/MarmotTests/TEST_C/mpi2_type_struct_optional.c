// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_mpi2_type_struct_optional 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Send@.*: Argument 2 [(]count[)] is zero, which is correct but unusual!}}
// REQUIRES: HAVE_MPI_WCHAR, HAVE_MPI_SIGNED_CHAR

#include <stdio.h>
#include <mpi.h>
#include <stddef.h>

#define NUM_TYPES 5

int main(int argc, char** argv)
{
    int rank, size;
    MPI_Status status;
    MPI_Datatype tNew;
    int blocklens[NUM_TYPES] = {1, 1, 1, 1, 1};
    MPI_Aint displs[NUM_TYPES] = {
        0,
        sizeof(signed long long int),
        sizeof(signed long long int) * 2,
        sizeof(signed long long int) * 2 + sizeof(unsigned long long int),
        sizeof(signed long long int) * 2 + sizeof(unsigned long long int) + sizeof(wchar_t)};
    MPI_Datatype types[NUM_TYPES] =
        {MPI_LONG_LONG_INT, MPI_LONG_LONG, MPI_UNSIGNED_LONG_LONG, MPI_WCHAR, MPI_SIGNED_CHAR};

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 2) {
        printf("This test needs at least 2 ranks !");
    } else {
        MPI_Type_struct(NUM_TYPES, blocklens, displs, types, &tNew);
        MPI_Type_commit(&tNew);

        if (rank == 0) {
            MPI_Send(NULL, 0, MPI_LONG_LONG_INT, 1, 0, MPI_COMM_WORLD);
            MPI_Send(NULL, 0, tNew, 1, 33, MPI_COMM_WORLD);
        } else if (rank == 1) {
            MPI_Recv(NULL, 0, MPI_LONG_LONG_INT, 0, 0, MPI_COMM_WORLD, &status);
            MPI_Recv(NULL, 0, tNew, 0, 33, MPI_COMM_WORLD, &status);
        }

        MPI_Type_free(&tNew);

        MPI_Finalize();
    }

    return 0;
}
