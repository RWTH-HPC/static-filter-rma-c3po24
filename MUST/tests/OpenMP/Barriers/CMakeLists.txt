# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

foreach (MAIN_FILE_BASENAME
        DifferentBarriers_Error
        MultipleBarriers_NoError)
    addHybridTest("${MAIN_FILE_BASENAME}" "${MAIN_FILE_BASENAME}.cpp" 2
            ""
            "Error;ERROR;Warning;WARNING"
            ""
            ""
            )
endforeach ()
