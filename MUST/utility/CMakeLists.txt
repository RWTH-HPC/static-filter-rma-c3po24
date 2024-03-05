# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# Define the default MPI interface to be intercepted.
#
# By default, mustrun will intercept the MPI C interface, which should be fine
# for most recent MPI implementations (even for Fortran). However, if one needs
# to switch the interface to be intercepted many times, one can change the
# default interface by this switch, too.
SET(MUST_DEFAULT_MPI_INTERFACE "c"
    CACHE STRING "The default MPI interface to be intercepted.")
SET(MUST_USER_CACHE_DIR "~/.cache/must" 
    CACHE STRING "Default directory for user cache of MUST configurations")

set(MUST_TIMEOUT_SCRIPT "${CMAKE_INSTALL_PREFIX}/bin/timeout" CACHE INTERNAL "")
# Use the more robust timeout executable provided by the system if possible. Use
# the script provided by mustrun as fallback if timeout can't be found in the
# system path.
find_program(TIMEOUT_EXECUTABLE
    # gtimeout is for macOS users that installed coreutils via brew
    timeout gtimeout
    DOC "the timeout executable used by mustrun"
    )
if(TIMEOUT_EXECUTABLE)
    # We assume the cli of GNU coreutils timeout. This might not work for other
    # implementations of timeout.
    set(TIMEOUT_COMMAND
	"${TIMEOUT_EXECUTABLE} --kill-after 10 --preserve-status")
else()
    set(TIMEOUT_COMMAND "${MUST_TIMEOUT_SCRIPT} -t")
endif()

OPTION(MUSTRUN_INJECT_ENV "Mustrun should use env to export variables for each MPI process" TRUE)

if (MUST_SANITIZE_ASAN)
    find_library(ASAN_LIBRARIES
            NAMES asan
            HINTS ENV LIBRARY_PATH
            REQUIRED)
    message(STATUS "Using ${ASAN_LIBRARIES} for LD_PRELOAD")
endif()

# Configure mustrun.
#
# As the mustrun script needs paths of GTI binaries and one can't simply get
# these via the target properties (see CMake CMP0026), generator expressions
# will be used to get the location of the binaries. Therefore, mustrun will be
# configured first to replace all variables inside the script. Then, the
# generator expressions will be evaluated.
CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/mustrun2.sh"
               "${CMAKE_CURRENT_BINARY_DIR}/mustrun.configured" @ONLY)
FILE(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/mustrun"
              INPUT  "${CMAKE_CURRENT_BINARY_DIR}/mustrun.configured")

add_executable(must_tool_process must_dummy.c)
target_link_libraries(must_tool_process MPI::MPI_C)

add_library(PrintMemUsage SHARED
  printMemoryusage.c
)

add_library(XPrintMemUsage SHARED
  printMemoryusage.c
)

make_mpi_target(PrintMemUsage)
make_mpi_target(XPrintMemUsage)

if(APPLE)
    get_target_property(TEMP_FLAG XPrintMemUsage LINK_FLAGS)
    string(TOUPPER "${TEMP_FLAG}" TEMP_CAPITAL_FLAG)
    if(${TEMP_CAPITAL_FLAG} STREQUAL "TEMP_FLAG-NOTFOUND")
        set(TEMP_FLAG "")
    endif()
    set(TEMP_FLAG "${TEMP_FLAG} -undefined dynamic_lookup")
    set_target_properties(XPrintMemUsage PROPERTIES LINK_FLAGS ${TEMP_FLAG})
endif()

target_compile_definitions( XPrintMemUsage PRIVATE -DPMPIPREFIX=XMPI)

if(TARGET TSanRMAOptimizerPlugin)
    # Configure must-compile
    get_target_property(RMAOPT_CLANG_VERSION_MIN TSanRMAOptimizerPlugin REQUIRED_LLVM_VERSION_MIN)
    set(RMAOPT_OPT_CONFIGURE_SCRIPT "${CMAKE_INSTALL_PREFIX}/bin/rmaopt_get_config.sh")
    set(RMAOPT_INSTALLED_PLUGIN_PATH "${CMAKE_INSTALL_PREFIX}/lib/TSanRMAOptimizerPlugin.so")
    set(RMAOPT_INSTALLED_TSAN_LIB "${CMAKE_INSTALL_PREFIX}/lib/RMAOptimizerTSan/libclang_rt.tsan-x86_64.a")
    set(MUST_ONREPORT_PATH "${CMAKE_INSTALL_PREFIX}/lib/libonReportLoader.a")
    # must-cc
    set(COMPILER_WRAP_EXPR "\${MPICC:-mpicc}")
    set(MUST_COMPILE_SRC_FILE_ENDINGS "\\.c")
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/must-compile.sh.in"
                "${CMAKE_CURRENT_BINARY_DIR}/must-cc.configured" @ONLY)
    FILE(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/must-cc"
                INPUT  "${CMAKE_CURRENT_BINARY_DIR}/must-cc.configured")
    set(MUST_COMPILER_WRAPPER_CC_PATH "${CMAKE_INSTALL_PREFIX}/bin/must-cc" CACHE INTERNAL "")
    # must-cxx
    set(COMPILER_WRAP_EXPR "\${MPICXX:-mpicxx}")
    set(MUST_COMPILE_SRC_FILE_ENDINGS "\\.cpp|\\.cc|\\.cxx")
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/must-compile.sh.in"
                "${CMAKE_CURRENT_BINARY_DIR}/must-cxx.configured" @ONLY)
    FILE(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/must-cxx"
                INPUT  "${CMAKE_CURRENT_BINARY_DIR}/must-cxx.configured")
    set(MUST_COMPILER_WRAPPER_CXX_PATH "${CMAKE_INSTALL_PREFIX}/bin/must-cxx" CACHE INTERNAL "")
    # must-fort
    set(COMPILER_WRAP_EXPR "\${MPIFC:-mpifort}")
    set(MUST_COMPILE_SRC_FILE_ENDINGS "\\.f90|\\.f95|\\.f03|\\.f07")
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/must-compile.sh.in"
                "${CMAKE_CURRENT_BINARY_DIR}/must-fort.configured" @ONLY)
    FILE(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/must-fort"
                INPUT  "${CMAKE_CURRENT_BINARY_DIR}/must-fort.configured")
    set(MUST_COMPILER_WRAPPER_CXX_PATH "${CMAKE_INSTALL_PREFIX}/bin/must-fort" CACHE INTERNAL "")


    INSTALL(FILES
            "${CMAKE_CURRENT_BINARY_DIR}/must-cc"
            "${CMAKE_CURRENT_BINARY_DIR}/must-cxx"
            "${CMAKE_CURRENT_BINARY_DIR}/must-fort"
        DESTINATION bin 
        PERMISSIONS 
            OWNER_READ OWNER_WRITE OWNER_EXECUTE  
            GROUP_READ GROUP_EXECUTE  
            WORLD_READ WORLD_EXECUTE
        )
endif()

INSTALL(FILES
        "${CMAKE_CURRENT_BINARY_DIR}/mustrun"
        "timeout"
        "cleanup_shm"
        $<TARGET_FILE:must_tool_process>
    DESTINATION bin 
    PERMISSIONS 
        OWNER_READ OWNER_WRITE OWNER_EXECUTE  
        GROUP_READ GROUP_EXECUTE  
        WORLD_READ WORLD_EXECUTE
    )

INSTALL(TARGETS
	PrintMemUsage XPrintMemUsage
    DESTINATION modules
    PERMISSIONS 
        OWNER_READ OWNER_WRITE 
        GROUP_READ 
        WORLD_READ 
    )

INSTALL(
    FILES "error_generator.c" 
    DESTINATION src
    )

INSTALL(
    FILES "tsan_suppressions.txt"
    DESTINATION share
)
