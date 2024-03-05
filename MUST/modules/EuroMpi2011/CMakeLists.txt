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
        I_BarrierReduction.h
        I_EvaluateBarriers.h
        BarrierReductionApi.h
        BarrierReductionOp.h
    DESTINATION 
        include)

#Build and install modules 
GTI_MAC_ADD_MODULE (barrierReduction "BarrierReduction.cpp;${GTI_SOURCE_PATH}/CompletionTree.cpp;${BASE_SOURCES}" "CXX")
GTI_MAC_ADD_MODULE (evaluateBarriers "EvaluateBarriers.cpp;${BASE_SOURCES}" "CXX")
