# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

foreach(MAIN_FILE_BASENAME
        Locks_uninitialized
        Locks_no_error
        Locks_nested_no_error
        Locks_foreign_unlock
        Locks_double_init)
    addHybridTest ("${MAIN_FILE_BASENAME}" "${MAIN_FILE_BASENAME}.cpp" 2
            ""
            "Error;ERROR;Warning;WARNING"
            ""
            ""
            )
endforeach()
