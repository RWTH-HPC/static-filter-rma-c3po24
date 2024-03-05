/**
 * @file omp_coll_comm.c source file for a Program wich does incorrect
 *       usage of collective calls.
 * @date 13.09.07
 * @author Tobias Hilbrich
 *
 * This program is erroneus, it does:
 *  - All Processes do 4 MPI_Barrier calls simultanously (by using Threads)
 */

#include <mpi.h>
#include <omp.h>
#include <stdio.h>

#define NUM_THREADS 4

int main(int argc, char** argv)
{
    int rank, size, i, thread;

    int msg;

    int provided;

    //init MPI
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if ((rank == 0) && (provided != MPI_THREAD_MULTIPLE))
        printf("WARNING this MPI Implementation does not support MPI_THREAD_MULTIPLE\n");

    //set num threads and init message
    omp_set_num_threads(NUM_THREADS);

#pragma omp parallel private(thread)
    {
        MPI_Barrier(MPI_COMM_WORLD);
    }

    MPI_Finalize();

    return 0;
}
