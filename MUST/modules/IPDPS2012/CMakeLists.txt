# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for the modules/LostMessage directory.
#
# @author Tobias Hilbrich
# @date 06.05.2011

#Install headers
MUST_INSTALL(
    FILES 
        I_TransferStatistics.h
        I_TransferReduction.h
        I_TransferTracer.h
        TransferReductionApi.h
    DESTINATION 
        include)



#Build and install modules 
GTI_MAC_ADD_MODULE (transferStatistics "TransferStatistics.cpp;${BASE_SOURCES}" "CXX")
GTI_MAC_ADD_MODULE (transferReduction "TransferReduction.cpp;${GTI_SOURCE_PATH}/CompletionTree.cpp;${BASE_SOURCES}" "CXX")
GTI_MAC_ADD_MODULE (transferTracer "TransferTracer.cpp;${GTI_SOURCE_PATH}/CompletionTree.cpp;${BASE_SOURCES}" "CXX")
