// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_typestruct-64bit 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

/**
 *  @file
 *
 * On some clusters (64-bit) this code does not run:
 * The reason is that we define
 *   MPI_Aint      disp[4];
 *   int           base,i;
 *   ...
 *   base = disp[0];
 *   On some clusters (32-bit), MPI_Aint is int, and it works, unfortunately,
 *   on some clusters (64-bit), e.g. cacau, MPI_Aint is long, 
 *   and the code breaks.
 * This example can be found exactly like this in the MPI-1 standard, page 79,
 * Example 3.34.
 * 
 *  It just calls 
 *  - MPI_Init
 *  - MPI_Comm_rank
 *  - MPI_Comm_size
 *  - MPI_Type_struct
 *  - MPI_Type_commit
 *  - MPI_Type_free
 *  - MPI_Ssend
 *  - MPI_Recv
 *  - MPI_Finalize 
 *
 *  @author Bettina Krammer
 *
 *  $Id: typestruct-64bit.c 454 2006-02-10 13:45:19Z rusbetti $  
 */

#include <mpi.h>

int main(int argc, char** argv)
{
    struct Partstruct {
        int class; /* particle class */
        double d;  /* particle coordinates */
        char b;    /* some additional information */
    };

    struct Partstruct particle;

    int i, rank, size;
    MPI_Status status;

    /* build datatype describing structure */

    MPI_Datatype Particletype;
    MPI_Datatype type[3] = {MPI_INT, MPI_DOUBLE, MPI_CHAR};
    int blocklen[3] = {1, 1, 1};
    MPI_Aint disp[3];
    MPI_Aint base;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    /* compute displacements of structure components */

    MPI_Address(&particle, disp);
    MPI_Address(&particle.d, disp + 1);
    MPI_Address(&particle.b, disp + 2);
    base = disp[0];
    for (i = 0; i < 3; i++) {
        disp[i] -= base;
    }

    MPI_Type_struct(3, blocklen, disp, type, &Particletype);
    MPI_Type_commit(&Particletype);

    if (rank == 0) {
        for (i = 1; i < size; i++) {
            MPI_Recv(
                &particle,
                1,
                Particletype,
                MPI_ANY_SOURCE,
                MPI_ANY_TAG,
                MPI_COMM_WORLD,
                &status);
        }

    } else {
        MPI_Ssend(&particle, 1, Particletype, 0, 17, MPI_COMM_WORLD);
    }

    MPI_Type_free(&Particletype);
    MPI_Finalize();

    return 0;
}
