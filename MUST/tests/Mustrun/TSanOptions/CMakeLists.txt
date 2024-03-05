# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause


addTestExecutable("mustrun_tsan_options" "mustrun_tsan_options.c")

add_compile_options("-fsanitize=thread")
addHybridTest ("mustrun_tsan_suppress_race" "mustrun_tsan_suppress_race.c" 1
    "data race"
    ""
    "-fsanitize=thread"
    "-fsanitize=thread"
    )

configure_file(mytsan_suppressions.txt mytsan_suppressions.txt @ONLY)
