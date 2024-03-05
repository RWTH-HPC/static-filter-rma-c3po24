# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

add_library(MsgFilterTest OBJECT ${CMAKE_SOURCE_DIR}/modules/MustBase/MsgFilter.cpp MsgFilterTest.cpp)
target_link_libraries(MsgFilterTest PRIVATE ${gtest} MustCommon)
target_include_directories(MsgFilterTest PRIVATE ${CMAKE_SOURCE_DIR}/modules/MustBase ${CMAKE_BINARY_DIR}/modules/MustBase)

target_link_libraries(UnitTests PRIVATE MsgFilterTest)
