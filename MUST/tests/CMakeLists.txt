# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for tests directory.
#
# @author Tobias Hilbrich
# @date 03.01.2011
#
# The tests have to be used as follows:
#    ccmake       #configure with CMake, enable the test option there, generate
#    make         #build
#    make install #install and patch the libraries
#    make check    #run the tests
#
# Currently likely not very portable, should work well with OpenMPI atm.
#

include(CMakeDependentOption)
include(MustTestFeatures)

set_must_test_features()

OPTION(MUST_LIT_FORWARD_FULL_ENV "Lit should forward all environmental variables to test execution" FALSE)

##################
#Recurse into sub directories

#Add Include directories for modules
INCLUDE_DIRECTORIES(
    "${CMAKE_BINARY_DIR}/modules/Common" #For results of feature tests
    "${CMAKE_SOURCE_DIR}/modules/MustBase"  #For MustParallelId and the like
    "${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Comm"
    "${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Datatype"
    "${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Err"
    "${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Group"
    "${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Keyval"
    "${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Op"
    "${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Request"
    "${CMAKE_SOURCE_DIR}/modules/ResourceTracking/Utility"
    "${CMAKE_SOURCE_DIR}/modules/Common"
    "${GTI_INCLUDE_PATH}" # For I_Module and the like
    "${GTI_SOURCE_PATH}"  # For gti::ModuleBase and the like
    ${PnMPI_INCLUDE_DIR}  # Used by gti::ModuleBase
    ${MPI_C_INCLUDE_PATH}      # For mpi.h
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_BINARY_DIR}/include # For GTI_Annotations.h
    )


# Set up lit based tests
set(shipped_lit_executable "${CMAKE_SOURCE_DIR}/externals/llvm-lit/lit.py")
if (EXISTS "${shipped_lit_executable}")
    # Use shipped lit if available but still respect user-defined paths to the executable.
    set(LLVM_LIT_PATH "${shipped_lit_executable}" CACHE FILEPATH "llvm-lit executable.")
endif()
unset(shipped_lit_executable)

find_package(LIT 12.0.0)

if(NOT LIT_FOUND)
    message(SEND_ERROR "Could not find llvm's lit executable needed for testing. \
Please make sure that either the LLVM package is in your CMake module search \
path or that the executable is in your PATH environment variable. \
Alternatively you might want to set the CMake variable LLVM_TOOLS_BINARY_DIR \
to the path containing the lit executable.")
endif()


# Set up FileCheck
set(shipped_filecheck_executable "$<TARGET_FILE:FileCheck_Standalone>")
set(MUST_BUILD_FILECHECK FALSE)
if (TARGET FileCheck_Standalone)
    # Use shipped FileCheck if available but still respect user-defined paths to the binary.
    set(LLVM_FILECHECK_PATH "${shipped_filecheck_executable}" CACHE FILEPATH "Filecheck executable.")

    if ("${LLVM_FILECHECK_PATH}" STREQUAL "${shipped_filecheck_executable}")
        set(MUST_BUILD_FILECHECK TRUE)
    endif()
endif()

find_package(FileCheck)

if(NOT FileCheck_FOUND)
    message(SEND_ERROR "Could not find llvm's FileCheck executable needed for \
testing. Please make sure that either the LLVM package is in your CMake \
module search path or that the executable is in your PATH environment \
variable. Alternatively you might want to set the CMake variable \
LLVM_TOOLS_BINARY_DIR to the path containing the FileCheck executable.")
endif()

# Place modules built by tests in common directory. It will be added by lit.cfg to the PNMPI_LIB_PATH envvar.
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib")
set(LIBRARY_OUTPUT_PATH "${CMAKE_CURRENT_BINARY_DIR}/lib")
set(MUST_PREBUILD_PATH "${PROJECT_BINARY_DIR}/prebuild/prebuilds:${CMAKE_CURRENT_BINARY_DIR}/prebuilds")

#Build base sources variable for modules
SET (MUSTRUN "${CMAKE_BINARY_DIR}/utility/mustrun")
set(MUST_TEST_MPIC_COMPILER "mpicc" )
set(MUST_FILECHECK_EXECUTABLE "${LLVM_FILECHECK_PATH}")
set(MUST_TEST_FLAGS "-rdynamic")
set(MUST_MUSTRUN_EXECUTION_FLAGS "--must:mpiexec orterun --must:output stdout" )
set(MUST_LIT_EXECUTABLE "${LLVM_LIT_PATH}")

set(TESTS_WORKERS 1 CACHE STRING "Sets the amount of workers when running tests. (Note that this only sets the amount of lit workers of which each might spawn more processes.)")
set(MUST_LIT_PASS_PREFIXED_VARIABLES "" CACHE STRING "List of environmental variable prefixes to be forwarded through lit (e.g. SLURM_)")

OPTION (TESTS_DISABLE_MUST_CLEAN "Selects whether the mustrun option '--must:clean' is disabled during test runs." False)
cmake_dependent_option(TEST_PREBUILDS
        "Selects whether the prebuilds for tests should be generated. This accelerates test execution."
        ON
        "ENABLE_PREBUILD"
        OFF
        )

OPTION (MUST_FAST_TESTS_ONLY "Run only fast executing tests." False)

set(MUST_MPI_HAS_ASSERTIONS False CACHE BOOL "Set this to true when the MPI library itself has builtin assertions.")
mark_as_advanced(FORCE MUST_MPI_HAS_ASSERTIONS)

set(compilers GNU Clang)
if (CMAKE_CXX_COMPILER_ID IN_LIST compilers)
    add_compile_options("$<$<COMPILE_LANGUAGE:C>:-Wno-comment>")
    add_compile_options("$<$<COMPILE_LANGUAGE:CXX>:-Wno-comment>")
endif()
unset(compilers)

# Recurse into all subdirectories.
set(suites
        Annotations
        basic
        BasicChecks
        BufferChecks
        CommChecks
        CommGroupTrack
        DatatypeChecks
        DatatypeTrack
        DeadlockDetection
        ErrTrack
        ErrorFilter
        FortranTests
        GroupChecks
        Hybrid
        IntegrityChecks
        KeyvalTrack
        LeakChecks
        OneSidedChecks
        MustBase
        Mustrun
        MpiTSan
        OnlyOnRootCondition
        OpenMP
        OpTrack
        OverlapChecks
        PartitionedP2PChecks
        RequestChecks
        RequestTrack
        TypeArt
        TypeMatch
        VectorClock
        unit
        Unsupported
        )

IF (ENABLE_OLD_TESTS)
  list(APPEND suites old-tests)
ENDIF ()

function(MUST_TEST_PREBUILD_CONFIGURATION)
    MUST_PREBUILD_CONFIGURATION(${ARGN} EXCLUDE_FROM_PREBUILDS DEST ${CMAKE_BINARY_DIR}/tests/prebuilds)
    add_dependencies(build-test-prebuilt-${CURRENT_TEST_SUITE} ${ARGV0})
endfunction()

add_custom_target(build-test)
if (TARGET FileCheck_Standalone AND MUST_BUILD_FILECHECK)
    add_dependencies(build-test FileCheck_Standalone)
endif()
FOREACH (suite ${suites})
    add_custom_target(build-test-${suite})
    add_custom_target(build-test-prebuilt-${suite})
    add_dependencies(build-test build-test-${suite} build-test-prebuilt-${suite})

    set(CURRENT_TEST_SUITE ${suite})
    add_subdirectory(${suite})
    unset(CURRENT_TEST_SUITE)

    must_add_check_target(SUITES ${suite} DEPENDS build-test-${suite} build-test-prebuilt-${suite})
ENDFOREACH ()

# Special check targets that do not correspond to a directory
must_add_check_target(NAME check SUITES . COMMENT "Run all MUST tests" DEPENDS build-test)
must_add_check_target(NAME check-fast SUITES . FAST_TESTS COMMENT "Run all fast MUST tests" DEPENDS build-test)

add_custom_target(build-test-minimal)
add_dependencies(build-test-minimal
          build-test-basic  # This also builds the test module
          sendDl
          DDlsendDl
          build-test-OpTrack  # This also builds the test module
          AnnotationWarningIfZero
          ErrorFilter_test1
)

# Small set of tests to check basic build & run functionality.
must_add_check_target(
        NAME check-minimal
        COMMENT "Run a small selection of tests"
        SUITES
          basic/initialized.c  # simple test without layout
          basic/basic.c  # simple test with layout
          MpiTSan/send.c  # non-distributed
          MpiTSan/bcast.c  # distributed
          DeadlockDetection/DeadlockDetection/sendDl.c  # local & distributed deadlock
          Hybrid/CommSizeOnWorkerThread.cpp  # basic hybrid check
          FortranTests/simple_fortran.f  # fortran based test
          OpTrack/testOpTrack.c  # Track test
          Annotations/AnnotationWarningIfZero.cpp  # GTI Annotations
          TypeArt/DistributedStructError.c  # TypeArt
          ErrorFilter/single_error.c  # Message filter
          OpenMP/Sanity/ThreadLevel_single_parallel.cpp
        DEPENDS
          build-test-minimal
)

if (OpenMP_FOUND)
    add_dependencies(build-test-minimal ThreadLevel_single_parallel)
ENDIF ()
if (HAVE_TSAN AND OpenMP_FOUND)
    add_dependencies(build-test-minimal MpiTSanSend MpiTSanBcast)
ENDIF ()
# IF (GTI_IS_THREAD_SAFE AND OpenMP_FOUND)
#     add_dependencies(build-test-minimal CommSizeOnWorkerThread)
# ENDIF ()
IF (GTI_ENABLE_FORTRAN)
    add_dependencies(build-test-minimal simple_fortran)
ENDIF ()



set(features_used_in_lit
    HAVE_MPI_TYPE_CREATE_RESIZED
    HAVE_MPI_TYPE_CREATE_INDEXED_BLOCK
    HAVE_MPI_ALLTOALLW
    HAVE_MPI_EXSCAN
    HAVE_MPI_REDUCE_SCATTER_BLOCK
    HAVE_MPI_REDUCE_SCATTER
    HAVE_MPI_WCHAR
    HAVE_MPI_SIGNED_CHAR
    HAVE_MPI_IALLGATHER
    HAVE_MPI_IALLGATHERV
    HAVE_MPI_IALLREDUCE
    HAVE_MPI_IALLTOALL
    HAVE_MPI_IALLTOALLV
    HAVE_MPI_IALLTOALLW
    HAVE_MPI_IBARRIER
    HAVE_MPI_IBCAST
    HAVE_MPI_IGATHER
    HAVE_MPI_IGATHERV
    HAVE_MPI_IREDUCE
    HAVE_MPI_IREDUCE_SCATTER
    HAVE_MPI_IREDUCE_SCATTER_BLOCK
    HAVE_MPI_ISCAN
    HAVE_MPI_IEXSCAN
    HAVE_MPI_ISCATTER
    HAVE_MPI_ISCATTERV
    HAVE_MPI_COMM_IDUP_WITH_INFO
    HAVE_MPI_COMM_CREATE_FROM_GROUP
    HAVE_MPI_KEYVAL_CREATE
    HAVE_MPI_BARRIER_INIT
    HAVE_MPI_PREADY_LIST
    HAVE_MPI_PREADY_RANGE
    HAVE_MPI_PREADY
    HAVE_MPI_PRECV_INIT
    HAVE_MPI_PSEND_INIT
    HAVE_MPI_PARRIVED
)

set(variables_used_in_lit
    ${features_used_in_lit}
    GTI_IS_THREAD_SAFE
    OpenMP_FOUND
    OMPT_FOUND
    TESTS_DISABLE_MUST_CLEAN
    DISABLE_MARMOT_TESTS
    DISABLE_UMPIRE_TESTS
    ENABLE_OLD_TESTS
    GTI_ENABLE_FORTRAN
    ENABLE_Fortran_MIXED_TESTS
    ENABLE_Fortran_TESTS
    MUST_FAST_TESTS_ONLY
    MUST_MPI_HAS_ASSERTIONS
    ENABLE_TYPEART
    HAVE_TSAN
    ENABLE_STACKTRACE
    MUST_LIT_FORWARD_FULL_ENV
    MUST_SANITIZE_UBSAN
    MUST_SANITIZE_ASAN
)
set(LIT_FEATURE_DICT "")
foreach(var ${variables_used_in_lit})
    must_pythonize_bool(${var})
endforeach()
foreach(var ${features_used_in_lit})
    set(LIT_FEATURE_DICT "${LIT_FEATURE_DICT}'${var}':${${var}}, ")
endforeach()

set(NOT_HELPER_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/utils/not")

configure_file(lit.site.cfg.in lit.site.cfg.configured @ONLY)
file(GENERATE
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg"
        INPUT "${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.configured")
