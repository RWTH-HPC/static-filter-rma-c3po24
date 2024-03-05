# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

add_library(GroupTableTest OBJECT
        ${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Utility/HandleInfoBase.cpp
        ${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Group/Group.cpp
        ${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Group/GroupTable.cpp
        GroupTableTest.cpp
        )
target_link_libraries(GroupTableTest 
        PRIVATE
        ${gtest}
        )
target_include_directories(GroupTableTest
        PRIVATE
        ${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Group
        )

target_link_libraries(UnitTests PRIVATE GroupTableTest)
