/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "mpi.h"

int main(int argc, char** argv)
{
    int rank, size, arr[25];
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Datatype rowType, colType;
    MPI_Request request;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &size);

    //1) Create col/row datatypes for 5x5 matrix
    MPI_Type_contiguous(5, MPI_INT, &rowType);
    MPI_Type_vector(5, 1, 5, MPI_INT, &colType);
    MPI_Type_commit(&rowType);
    MPI_Type_commit(&colType);

    //2) Use MPI_Isend and MPI_Recv to perform a ring communication
    MPI_Isend(&arr[0], 1, colType, (rank + 1) % size, 456, comm, &request);
    MPI_Recv(&arr[10], 1, rowType, (rank - 1 + size) % size, 456, comm, &status);

    //3) Use MPI_Send and MPI_Recv to acknowledge the last recieve
    MPI_Send(arr, 0, MPI_INT, (rank - 1 + size) % size, 345, comm);
    MPI_Recv(arr, 0, MPI_INT, (rank + 1) % size, 345, comm, &status);

    MPI_Finalize();
}
