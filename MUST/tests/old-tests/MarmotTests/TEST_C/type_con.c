// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_type_con \
// RUN: 2>&1 \
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
 *  - MPI_Comm_size
 *  - MPI_Type_contiguous
 *  - MPI_Type_commit
 *  - MPI_Reduce
 *  - MPI_Type_free
 *
 *  @author Bettina Krammer, Katrin Bidmon, Matthias Mueller
 *
 *  $Id: type_con.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char* argv[])
{
    const int COUNT = 1;

    int root = -1;
    int ret1 = 0;
    int ret2 = 0;
    int ret3 = 0;
    int ret4 = 0;
    int dummy = 0;
    int retcom1 = 0;
    int retcom2 = 0;
    int globalres = 0;
    int numnode = 0;
    int mynode = 0;

    MPI_Comm comm;

    MPI_Datatype dat1;
    MPI_Datatype dat2;
    MPI_Datatype dat3;
    MPI_Datatype dat4;

    MPI_Init(&argc, &argv);
    root = 0;
    comm = MPI_COMM_WORLD;

    MPI_Comm_size(MPI_COMM_WORLD, &numnode);
    MPI_Comm_rank(MPI_COMM_WORLD, &mynode);

    if (mynode == root) {
        printf("Checking for MPI_Type_contiguous.........");
    }

    /* Test1: create derived datatype from basic datatype */
    ret1 = MPI_Type_contiguous(2, MPI_INT, &dat1);
    retcom1 = MPI_Type_commit(&dat1);

    /* Test2: create new datatype from derived datatype (1.level ) */
    ret2 = MPI_Type_contiguous(2, dat1, &dat2);
    retcom2 = MPI_Type_commit(&dat2);

    /* Test3: create new datatype from derived datatype (2. level) */
    ret3 = MPI_Type_contiguous(2, dat2, &dat3);

    /* Test4: create new datatype from derived datatype (1.level) */

    ret4 = MPI_Type_contiguous(4, dat1, &dat4);

    if ((ret1 == MPI_SUCCESS) && (ret2 == MPI_SUCCESS) && (ret3 == MPI_SUCCESS) &&
        (ret4 == MPI_SUCCESS)) {
        dummy = 1;
    } else {
        dummy = 0;
    }

    MPI_Reduce(&dummy, &globalres, COUNT, MPI_INT, MPI_MIN, root, comm);

    if (mynode == root) {
        if (globalres == 1) {
            printf("working\n");
        } else {
            printf("false\n");
        }
        printf("Checking for MPI_Type_commit.............");
    }

    if ((retcom1 == MPI_SUCCESS) && (retcom2 == MPI_SUCCESS)) {
        dummy = 1;
    } else {
        dummy = 0;
    }

    MPI_Reduce(&dummy, &globalres, COUNT, MPI_INT, MPI_MIN, root, comm);

    if (mynode == root) {
        if (globalres == 1) {
            printf("working\n");
        } else {
            printf("false\n");
        }
        printf("Checking for MPI_Type_free...............");
    }

    ret1 = MPI_Type_free(&dat1);
    ret2 = MPI_Type_free(&dat2);
    ret3 = MPI_Type_free(&dat3);
    ret4 = MPI_Type_free(&dat4);

    if ((ret1 == MPI_SUCCESS) && (ret2 == MPI_SUCCESS) && (ret3 == MPI_SUCCESS) &&
        (ret4 == MPI_SUCCESS)) {
        dummy = 1;
    } else {
        dummy = 0;
    }

    MPI_Reduce(&dummy, &globalres, COUNT, MPI_INT, MPI_MIN, root, comm);

    if (mynode == root) {
        if (globalres == 1) {
            printf("working\n");
        } else {
            printf("false\n");
        }
    }

    MPI_Finalize();

    return 0;
}
