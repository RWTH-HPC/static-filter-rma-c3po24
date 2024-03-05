# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for modules of MPI communicator and group tracking test.
#
# @author Tobias Hilbrich
# @date 04.03.2011
#

#Build and install modules 
must_add_test_module (printCommGroup "PrintCommGroup.cpp" "CXX")

