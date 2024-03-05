# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for unsupported feature tests.
#
# @author Felix Tomski
# @date 12.07.2022
#

##################
#Recurse into sub directories
ADD_SUBDIRECTORY(mpi3)

#Only go into MPI-4 tests if we got all of them
IF (MPI_C_VERSION VERSION_GREATER_EQUAL 4)
    ADD_SUBDIRECTORY(mpi4)
ENDIF ()
