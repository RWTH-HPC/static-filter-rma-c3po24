// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_twc 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

/**
 *  @file
 *
 *  This program is erroneous, but not because of an MPI bug.
 *
 *  It just calls
 *  - MPI_Init
 *  - MPI_Finalize
 *  - MPI_Comm_rank
 *  - MPI_Comm_size
 *  - MPI_Barrier
 *  - MPI_Waitsome
 *  - MPI_Irecv
 *  - MPI_Isend
 *
 *  @author Bernd Mohr
 *
 *  $Id: twc.c 997 2009-10-02 14:44:28Z hpczink $
 */

#include <stdio.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <unistd.h>
#endif

#include <mpi.h>

#define NUMMSG 3

/* -- Waitsome --------------------------------------------------------- */
void waitsome(int me, int sz)
{
    int i, j, cnt, idx;
    MPI_Request req[NUMMSG];
    MPI_Status stat[NUMMSG];
    int idxs[NUMMSG];
    int buf[NUMMSG];

    for (i = 0; i < NUMMSG; ++i) {
        if (me % 2 == 0) {
            buf[i] = i;
            MPI_Isend(buf + i, 1, MPI_INT, (me + 1 + sz) % sz, i, MPI_COMM_WORLD, req + i);
#ifdef _WIN32
            Sleep(1000);
#else
            sleep(1);
#endif
        } else {
            buf[i] = 0;
            MPI_Irecv(buf + i, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, req + i);
        }
    }

    i = 0;
    while (i < NUMMSG) {
        MPI_Waitsome(NUMMSG, req, &cnt, idxs, stat);
        i += cnt;
        if (me == 1) {
            for (j = 0; j < cnt; ++j) {
                idx = idxs[j];
                fprintf(
                    stderr,
                    "Waitsome: idx %d got %d from %d tag %d\n",
                    idx,
                    buf[idx],
                    stat[idx].MPI_SOURCE,
                    stat[idx].MPI_TAG);
            }
        }
    }

    for (i = 0; i < NUMMSG; ++i) {
        if (buf[i] != i)
            fprintf(stderr, "%d: Waitsome: buf[%d] = %d\n", me, i, buf[i]);
    }
    MPI_Barrier(MPI_COMM_WORLD);
}

/* -- main ------------------------------------------------------------- */
int main(int argc, char* argv[])
{
    int me, sz;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &me);
    MPI_Comm_size(MPI_COMM_WORLD, &sz);

    waitsome(me, sz);

    MPI_Finalize();

    return 0;
}
