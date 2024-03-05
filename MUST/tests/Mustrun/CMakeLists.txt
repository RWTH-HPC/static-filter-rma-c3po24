# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

addTestExecutable(mustrun_passes_correct_envvars mustrun_passes_correct_envvars.c)
addTestExecutable(mustrun_numproc_opt mustrun_numproc_opt.cpp)
addTestExecutable(NegativeSwitches NegativeSwitches.cpp)

configure_file(layout.xml layout.xml @ONLY)
configure_file(layout-hybrid.xml layout-hybrid.xml @ONLY)

add_subdirectory(MsgLogger)
add_subdirectory(TSanOptions)
add_subdirectory(UserCache)