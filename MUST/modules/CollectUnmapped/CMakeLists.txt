# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause
#
# @file CMakeLists.txt
#       CMake file for modules of directory modules/CollectUnmapped.
#
# @author Felix Tomski
# @date 07.07.2022
#

#Install headers
MUST_INSTALL(FILES I_CollectUnmapped.h DESTINATION include)


#Build and install modules 
GTI_MAC_ADD_MODULE (collectUnmapped "CollectUnmapped.cpp;${BASE_SOURCES}" "CXX")
