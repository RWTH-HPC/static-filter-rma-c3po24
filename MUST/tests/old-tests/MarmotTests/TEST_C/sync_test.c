/** 
 * @file sync_test.c source file for OpenMP/MPI Hybrid program that tests for MARMOT threadsafety.
 * @date 18.05.07
 * @author Tobias Hilbrich
 */

#include <mpi.h>
#include <omp.h>
#include <stdio.h>
#include <malloc.h>

#define NUM_THREADS 4
#define NUM_ITERATIONS 10

int main(int argc, char** argv)
{
    int rank, size;
    int thread, threadsize;
    int i;

    int msg;

    MPI_Status status;
    MPI_Request req;
    int provided;

    //init MPI
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if ((rank == 0) && (provided != MPI_THREAD_MULTIPLE))
        printf("WARNING this MPI Implementation does not support MPI_THREAD_MULTIPLE\n");

    //set num threads and init message
    omp_set_num_threads(NUM_THREADS);

    //do a parallel for loop
    //communication scheme:
    //   * each process (with num k) sends NUM_ITERATIONS messages
    //     to the process (k+1)%size
    //   * the iterations are shared by the threads present
#pragma omp parallel for private(thread, threadsize, msg, req, status)
    for (i = 0; i < NUM_ITERATIONS; i++) {
        thread = omp_get_thread_num();
        threadsize = omp_get_num_threads();

        printf("Process %d/%d Thread %d/%d Iteration %d !\n", rank, size, thread, threadsize, i);

        if (rank % 2 == 0) {
            msg = thread * threadsize + rank * size;
            MPI_Isend(&msg, 1, MPI_INT, (rank + 1) % size, 0, MPI_COMM_WORLD, &req);
            MPI_Wait(&req, &status);
        } else {
            MPI_Recv(&msg, 1, MPI_INT, (rank - 1) % size, 0, MPI_COMM_WORLD, &status);
        }
    }

    MPI_Finalize();

    return 0;
}
