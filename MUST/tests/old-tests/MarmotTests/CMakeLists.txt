# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for deadlock detection tests.
#
# @author Mathias Korepkat
# @date 10.01.2012
#

IF (DISABLE_MARMOT_TESTS)
    RETURN()
ENDIF ()


##################

#Recurse into sub directories
ADD_SUBDIRECTORY(TEST_C)
#ADD_SUBDIRECTORY(TEST_C_F)
#ADD_SUBDIRECTORY(TEST_F)