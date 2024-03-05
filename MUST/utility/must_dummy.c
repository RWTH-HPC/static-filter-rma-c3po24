/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <mpi.h>
#include <stdlib.h>

int main()
{
    int provided;
    MPI_Init_thread(NULL, NULL, MPI_THREAD_SERIALIZED, &provided);
    MPI_Abort(MPI_COMM_WORLD, 1);
    return 0;
}
