/* -*- Mode: C; -*- */
// RUN: %must-run %mpiexec-numproc-flag 1 \
// RUN: %must-bin-dir/umpire_type-commit-twice 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Type_commit@.*0: Argument 1 [(]datatype[)] is already commited, there is no need to commit it again!}}

/* Creator: Jeffrey Vetter (vetter3@llnl.gov) Thu Feb 24 2000 */

/* type-commit-twice.c -- do a type commit twice w/ the same type */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#define buf_size 128

int main(int argc, char** argv)
{
    int nprocs = -1;
    int rank = -1;
    MPI_Comm comm = MPI_COMM_WORLD;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;
    MPI_Datatype newtype;

    /* init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("(%d) is alive on %s\n", rank, processor_name);
    fflush(stdout);

    MPI_Barrier(comm);

    MPI_Type_contiguous(128, MPI_INT, &newtype);
    MPI_Type_commit(&newtype);

    /* COMMITTING TWICE IS NOT AN ERROR - SEE:
     http://www.mpi-forum.org/docs/mpi-20-html/node50.htm#Node50
     AT MOST, UMPIRE SHOULD PROVIDE A CLEAR WARNING ABOUT MINOR 
     PERFORMANCE CONSEQUENCE (JUST A WASTED FUNCTION CALL)... */
    MPI_Type_commit(&newtype);

    MPI_Barrier(comm);

    MPI_Type_free(&newtype);

    printf("(%d) Finished normally\n", rank);
    MPI_Finalize();
    return 0;
}

/* EOF */
