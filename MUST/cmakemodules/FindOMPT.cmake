# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

include(FindPackageHandleStandardArgs)
include(CheckIncludeFile)

find_package(OpenMP)
if (OpenMP_FOUND)
    set(CMAKE_REQUIRED_FLAGS ${OpenMP_C_FLAGS})
    check_include_file(omp-tools.h OMPT_HEADER)
endif ()

find_package_handle_standard_args(OMPT DEFAULT_MSG OMPT_HEADER)
