# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

IF (TEST_PREBUILDS)
    MUST_TEST_PREBUILD_CONFIGURATION(prebuild-testOpenMP
            "--must:mpimode MPMD --must:openmp ${MPIEXEC_NUMPROC_FLAG} 4")
ENDIF ()

add_subdirectory(Adapter)
add_subdirectory(Barriers)
add_subdirectory(Locks)
add_subdirectory(Sanity)
