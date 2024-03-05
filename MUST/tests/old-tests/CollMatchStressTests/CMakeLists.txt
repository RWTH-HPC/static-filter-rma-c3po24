# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for EuroMPI2011 test case.
#
# @author Tobias Hilbrich
# @date 06.05.2011
#

###################
#Add test case
SET (tests "bcasts" "gathers" "reduces" "allgathers" "gathervs" "alltoalls" "alltoallvs")

IF (HAVE_MPI_IREDUCE)
    LIST(APPEND tests "ireduces")
ENDIF (HAVE_MPI_IREDUCE)

FOREACH (test ${tests})
    addTestExecutable ("${test}" "${test}.c")
ENDFOREACH(test)
