# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

foreach(MAIN_FILE_BASENAME
        ThreadLevel_NoParallel
        ThreadLevel_single
        ThreadLevel_single_parallel
        ThreadLevel_funneled_parallel
        ThreadLevel_funneled_parallel_error
        ThreadLevel_serialized_parallel
        ThreadLevel_serialized_parallel_error
        ThreadLevel_multiple
        ThreadLevel_safe_mpi_calls
)
    addHybridTest ("${MAIN_FILE_BASENAME}" "${MAIN_FILE_BASENAME}.cpp" 2
            ""
            "Error;ERROR;Warning;WARNING"
            ""
            ""
            )
endforeach()
