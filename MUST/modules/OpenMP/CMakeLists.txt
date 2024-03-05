# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# Check if the OpenMP tools interface is available. Otherwise this module can't
# be compiled and will be ignored.
IF (NOT OMPT_FOUND)
    RETURN()
ENDIF ()

add_subdirectory(Adapter)
add_subdirectory(BarrierChecks)
add_subdirectory(LockChecks)
add_subdirectory(SanityChecks)
