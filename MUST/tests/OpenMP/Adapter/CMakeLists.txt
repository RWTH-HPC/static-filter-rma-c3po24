# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

addHybridTest(warn_omp_before_mpi_init "warn_omp_before_mpi_init.cpp" 2
        ""
        "Error;ERROR;Warning;WARNING"
        ""
        "")
