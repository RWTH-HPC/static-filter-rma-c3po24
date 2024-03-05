// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_type_struct_valid1 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv)
{
    MPI_Datatype t1, t2;
    int blocklens[2] = {1, 1};
    MPI_Aint displs[2] = {0, sizeof(int) * 2};
    MPI_Datatype types[2];

    MPI_Init(&argc, &argv);

    MPI_Type_contiguous(2, MPI_INT, &t1);

    types[0] = t1;
    types[1] = t1;
    MPI_Type_struct(2, blocklens, displs, types, &t2);

    MPI_Type_free(&t1);
    MPI_Type_free(&t2);

    MPI_Finalize();

    return 0;
}
