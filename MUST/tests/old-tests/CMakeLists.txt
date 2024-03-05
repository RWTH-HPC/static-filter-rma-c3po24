# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.txt
#       CMake file for old-tests directory
#       Includes old tests that are disabled by default


IF (NOT ENABLE_OLD_TESTS)
    RETURN()
ENDIF ()

FILE(GLOB SUBDIRECTORIES "*")
FOREACH (DIR ${SUBDIRECTORIES})
    IF (EXISTS "${DIR}/CMakeLists.txt")
        ADD_SUBDIRECTORY(${DIR})
    ENDIF ()
ENDFOREACH ()
