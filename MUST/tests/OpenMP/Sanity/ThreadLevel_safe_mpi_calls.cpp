/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file
 * A test for thread safe mpi calls.
 *
 * Requests the threading level MPI_THREAD_FUNNELED and performs all MPI calls that are still
 * allowed to be called from any thread.
 *
 * This is a test for the analysis group OpenMPsanity.
 */

// REQUIRES: ompt
// RUN: env OMP_NUM_THREADS=4 %must-run --must:mpimode MPMD --must:openmp %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/ThreadLevel_safe_mpi_calls 2>&1 \
// RUN: | %filecheck --implicit-check-not \
// RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' --implicit-check-not 'BAD TERMINATION' %s

#include <array>
#include <mpi.h>

int main(int argc, char** argv)
{
    int provided = -1;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_FUNNELED, &provided);
#pragma omp parallel
    {
        {
            int init_flag = -1;
            MPI_Initialized(&init_flag);
        }

        {
            int fini_flag = -1;
            MPI_Finalized(&fini_flag);
        }

        {
            int thread_level = -1;
            MPI_Query_thread(&thread_level);
        }

        {
            int main_flag = -1;
            MPI_Is_thread_main(&main_flag);
        }

        {
            int version = -1;
            int subversion = -1;
            MPI_Get_version(&version, &subversion);
        }

        {
            std::array<char, MPI_MAX_LIBRARY_VERSION_STRING> version{};
            int version_str_len = -1;
            MPI_Get_library_version(version.data(), &version_str_len);
        }
    }

    MPI_Finalize();
    return 0;
}
