# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

GTI_MAC_ADD_MODULE(OpenMPlocks "OpenMPlocks.cpp;LockExtensions.cpp;LockTracker.cpp" "CXX")
target_link_libraries(OpenMPlocks MustCommon)

MUST_INSTALL(
        FILES
        I_OpenMPlocks.h
        DESTINATION
        include)
