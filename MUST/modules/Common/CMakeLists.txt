# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for the modules/Common directory.
#
# @author Tobias Hilbrich
# @date 04.02.2011

add_library(MustCommon SHARED PrefixedOstream.cpp ScopeGuard.cpp Command.cpp MustOutputdir.cpp)
target_include_directories(MustCommon PUBLIC . "${CMAKE_CURRENT_BINARY_DIR}")

#Install headers
MUST_INSTALL(
    FILES 
        MustEnums.h 
#         MustTypes.h
        MustDefines.h  
        mustFeaturetested.h 
    DESTINATION 
        include)

###Configure config.h
CONFIGURE_FILE(mustConfig.h.in "${CMAKE_CURRENT_BINARY_DIR}/mustConfig.h")
CONFIGURE_FILE(MustTypes.h.in "${CMAKE_CURRENT_BINARY_DIR}/MustTypes.h")

#Install headers
MUST_INSTALL(
    FILES 
        "${CMAKE_CURRENT_BINARY_DIR}/mustConfig.h"
        "${CMAKE_CURRENT_BINARY_DIR}/MustTypes.h"
    DESTINATION 
        include)

INSTALL(TARGETS
        MustCommon
        DESTINATION
        "${CMAKE_INSTALL_PREFIX}/lib"
        PERMISSIONS
        OWNER_READ OWNER_WRITE
        GROUP_READ
        WORLD_READ
        )
