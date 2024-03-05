// RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/marmot_c_twc2 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING|Program received \
// RUN: signal)}}' %s

/**
 *  @file
 *
 *  Corrected version of twc.c.
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
 *  $Id: twc2.c 319 2004-08-16 11:25:02Z rusbetti $  
 */

#include <stdio.h>
#include <unistd.h>

#include <mpi.h>

#define NUMMSG 5

/* -- Waitsome --------------------------------------------------------- */
void waitsome(int me, int sz)
{
    int i, j, cnt, idx;
    MPI_Request req[NUMMSG] = {
        MPI_REQUEST_NULL,
    };
    MPI_Status stat[NUMMSG];
    int idxs[NUMMSG];
    int buf[NUMMSG];
    int rvalue;

    for (i = 0; i < NUMMSG; ++i) {
        if (me % 2 == 0) {
            buf[i] = i;
            MPI_Isend(buf + i, 1, MPI_INT, (me + 1 + sz) % sz, i, MPI_COMM_WORLD, &(req[i]));
            sleep(1);
        } else {
            buf[i] = 0;
            MPI_Irecv(buf + i, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &(req[i]));
        }
    }

    i = 0;
    while (i < NUMMSG) {
        rvalue = MPI_Waitsome(NUMMSG, req, &cnt, idxs, stat);

        if (rvalue != MPI_SUCCESS) {
            printf("rvalue=%d\n", rvalue);
        }

        printf("Rank:%d: cnt:%d\n", me, cnt);
        i += cnt;
        if (me == 0) {
            printf(" rank 0 completed %d send requests\n", cnt);
        }
        if (me == 1) {
            for (j = 0; j < cnt; ++j) {
                idx = idxs[j];
                fprintf(
                    stderr,
                    "Waitsome: j:%d idx %d got %d from %d tag %d error:%d \n",
                    j,
                    idx,
                    buf[idx],
                    stat[j].MPI_SOURCE,
                    stat[j].MPI_TAG,
                    stat[j].MPI_ERROR);
                if (req[idx] != MPI_REQUEST_NULL) {
                    printf(" Error request is not set to request_null\n");
                }
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
