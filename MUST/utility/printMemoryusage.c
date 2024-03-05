/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <sys/resource.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mpi.h>

#ifndef PMPIPREFIX
#define PMPIPREFIX PMPI
#endif

#define PMPIZE_T(f, p) p##f
#define PMPIZE_H(f, p) PMPIZE_T(f, p)
#define PMPIZE(f) PMPIZE_H(f, PMPIPREFIX)

int PMPIZE(_Finalize)();
int MPI_Finalize()
{
    int rank, size, print_rank = 1, all = 0;
    char* print_str = getenv("MUST_PRINT_RANK");
    if (print_str != NULL) {
        if (strcmp("all", print_str) == 0)
            all = 1;
        else
            print_rank = atoi(print_str);
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    struct rusage r;
    getrusage(RUSAGE_SELF, &r);

    if (rank != 0) {
        PMPI_Gather(&r.ru_maxrss, 1, MPI_LONG, NULL, 1, MPI_LONG, 0, MPI_COMM_WORLD);
    } else {
        // Collect and sum up data on a single rank for ordered output
        long procMemory[size];
        long totalMemory = 0;

        PMPI_Gather(&r.ru_maxrss, 1, MPI_LONG, procMemory, 1, MPI_LONG, 0, MPI_COMM_WORLD);

        for (int j = 0; j < size; ++j) {
            totalMemory += procMemory[j];
        }

        printf("[MEMUSAGE] Total Memory Consumption (max_rss): %.3f MiB\n", totalMemory / 1024.0);
        for (int j = 0; j < size; ++j) {
            printf("[MEMUSAGE] Rank %d:\t %.3f MiB\n", j, procMemory[j] / 1024.0);
        }
    }

    return PMPIZE(_Finalize)();
}
