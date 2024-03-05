# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for modules of basic test.
#
# @author Tobias Hilbrich
# @date 03.01.2011
#

#Build and install modules 
must_add_test_module (testLocation "TestLocation.cpp" "CXX")
must_add_test_module (testLog "TestLog.cpp" "CXX")
