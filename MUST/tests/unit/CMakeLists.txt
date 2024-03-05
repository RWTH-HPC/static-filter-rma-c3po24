# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# Unit tests
find_package(GTest 1.8.1)
if(GTest_FOUND)
    find_package(LIT QUIET)
    if(NOT LLVM_LIT_VERSION VERSION_GREATER_EQUAL 14)
        # See: https://github.com/llvm/llvm-project/commit/14a793ab0589de48511455038f63f28ff2506d48
        message(STATUS "llvm-lit below version 14.0.0 is not compatible with current versions of GoogleTest. Disabling unit tests.")
        return()
    endif()
else()
    message(STATUS "Could not find the googletest library. Disabling unit tests.
Hint: To install googletest, run on
 - Fedora: dnf install 'cmake(GTest)'
 - Ubuntu: apt install libgtest-dev")
    return()
endif()

# The following imported googletest targets are deprecated since
# CMake 3.20 and will be renamed in future.
set(gtest GTest::GTest)
set(gtest_main GTest::Main)


set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ".")

add_executable(UnitTests) # sources are added in the test directories
target_link_libraries(UnitTests PRIVATE ${gtest_main} ${gtest} MustCommon)
add_dependencies(build-test-${CURRENT_TEST_SUITE} UnitTests)

add_subdirectory(modules)

configure_file(lit.cfg lit.cfg COPYONLY)
