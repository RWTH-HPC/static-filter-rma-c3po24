# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause
#

# Install headers
MUST_INSTALL(FILES I_TSanInterceptor.h TSanInterceptorApi.h DESTINATION include)


# Build and install modules 
GTI_MAC_ADD_MODULE (TSanInterceptor "TSanInterceptor.cpp;${BASE_SOURCES}" "CXX")
set_target_properties(TSanInterceptor
        PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED ON)
set_target_properties(TSanInterceptor
        PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED ON)

# Create tsanInterceptor libs
ADD_LIBRARY(tsanInterceptorStatic STATIC TSanIntercept.cpp)
ADD_LIBRARY(tsanInterceptorShared SHARED TSanIntercept.cpp)
INSTALL(TARGETS
     tsanInterceptorStatic
    tsanInterceptorShared
    DESTINATION 
        "${CMAKE_INSTALL_PREFIX}/lib"
    PERMISSIONS 
        OWNER_READ OWNER_WRITE 
        GROUP_READ 
        WORLD_READ
    )
