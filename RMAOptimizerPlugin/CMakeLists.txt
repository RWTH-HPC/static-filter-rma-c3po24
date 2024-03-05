cmake_minimum_required(VERSION 3.15)

project(RMAOptimizerPlugin LANGUAGES CXX C)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

##
## Parse requirements
##

find_package(LLVM REQUIRED CONFIG)
set(RMAOPT_CLANG_VERSION_MIN 15)
if (${LLVM_VERSION_MAJOR} LESS_EQUAL ${RMAOPT_CLANG_VERSION_MIN})
  message("LLVM/Clang version is unsupported, minimum required is ${RMAOPT_CLANG_VERSION_MIN}!" FATAL_ERROR)
else()
  message("Detected LLVM/Clang version ${LLVM_VERSION_MAJOR}")
endif()

##
## Configure main project
##

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

add_llvm_pass_plugin(TSanRMAOptimizerPlugin
  Passes/src/Registrar.cpp
  Passes/src/TSanRMAOptimizer.cpp
  Passes/src/MPIUsageAnalysis.cpp
  Passes/src/TSanActivatorPass.cpp
  Passes/src/TSanClusteringPass.cpp
  Passes/src/TSanAttrPass.cpp
  Passes/src/ClusterModuleShim.cpp
  Passes/src/ThreadSanitizerMOD.cpp
  Passes/src/TSanInstrFilter.cpp
)

set_target_properties(TSanRMAOptimizerPlugin PROPERTIES REQUIRED_LLVM_VERSION_MIN ${RMAOPT_CLANG_VERSION_MIN})

##
## Configure compiler-rt
##

set(COMPILER_RT_STANDALONE_BUILD ON CACHE BOOL "" FORCE)
set(COMPILER_RT_BUILD_BUILTINS OFF CACHE BOOL "" FORCE)
set(COMPILER_RT_BUILD_CRT OFF CACHE BOOL "" FORCE)
set(COMPILER_RT_BUILD_XRAY OFF CACHE BOOL "" FORCE)
set(COMPILER_RT_BUILD_LIBFUZZER OFF CACHE BOOL "" FORCE)
set(COMPILER_RT_BUILD_PROFILE OFF CACHE BOOL "" FORCE)
set(COMPILER_RT_BUILD_MEMPROF OFF CACHE BOOL "" FORCE)
set(COMPILER_RT_BUILD_ORC OFF CACHE BOOL "" FORCE)
set(COMPILER_RT_BUILD_GWP_ASAN OFF CACHE BOOL "" FORCE)
set(COMPILER_RT_SANITIZERS_TO_BUILD tsan CACHE STRING "" FORCE)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/external/compiler-rt/compiler-rt compiler-rt)

set(RMA_BUILD_TARGETS TSanRMAOptimizerPlugin clang_rt.tsan-dynamic-x86_64 clang_rt.tsan-x86_64 clang_rt.tsan_cxx-x86_64 tsan)
foreach(rma_target IN LISTS RMA_BUILD_TARGETS)
  set_target_properties(${rma_target} PROPERTIES
                        LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin
                        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin
                        ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin
  )
endforeach()

##
## Convenience function for compile-time optimization selection
##

function(RMAOPT_GET_REQUIREMENTS_FOR_OPTIMIZATIONS optlist)
  set(RMAOPT_REQUIRED_PASSES_FOR_OPT "tsanMOD-module,function(tsanMOD)")
  set(RMAOPT_REQUIRED_OPTIONS_FOR_OPT "")

  # Pre-TSan Passes and Options
  if("AL" IN_LIST optlist OR "ALX" IN_LIST optlist)
    set(RMAOPT_REQUIRED_OPTIONS_FOR_OPT "-tsanMOD-use-optallowlist=1")
    if("ALX" IN_LIST optlist)
      set(RMAOPT_REQUIRED_OPTIONS_FOR_OPT "${RMAOPT_REQUIRED_OPTIONS_FOR_OPT} -tsanMOD-use-optallowlist-remoteaccesstypeext=1")
    endif()
  endif()

  # Post-TSan Passes and Options
  if("CLUSTER" IN_LIST optlist)
    set(RMAOPT_REQUIRED_PASSES_FOR_OPT "${RMAOPT_REQUIRED_PASSES_FOR_OPT},tsanMOD-cluster-shim,function(tsanMOD-clusteringopt)")
  endif()

  # Decide on attrpass
  if(NOT ("AL" IN_LIST optlist OR "ALX" IN_LIST optlist))
    set(RMAOPT_REQUIRED_PASSES_FOR_OPT "function(tsanMOD-attrpass),${RMAOPT_REQUIRED_PASSES_FOR_OPT}")
  endif()

  # Write back to parent
  set(RMAOPT_REQUIRED_PASSES_FOR_OPT "${RMAOPT_REQUIRED_PASSES_FOR_OPT}" PARENT_SCOPE)
  set(RMAOPT_REQUIRED_OPTIONS_FOR_OPT "${RMAOPT_REQUIRED_OPTIONS_FOR_OPT}" PARENT_SCOPE)
endfunction()


##
## Install files needed for runtime
##

install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/utils/rmaopt_get_config.sh" DESTINATION "bin/" PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ WORLD_EXECUTE WORLD_READ GROUP_EXECUTE GROUP_READ)
install(FILES
          "${CMAKE_CURRENT_BINARY_DIR}/bin/libclang_rt.tsan-x86_64.so"
          "${CMAKE_CURRENT_BINARY_DIR}/bin/libclang_rt.tsan-x86_64.a"
          "${CMAKE_CURRENT_BINARY_DIR}/bin/libclang_rt.tsan-x86_64.a.syms"
          "${CMAKE_CURRENT_BINARY_DIR}/bin/libclang_rt.tsan_cxx-x86_64.a"
          "${CMAKE_CURRENT_BINARY_DIR}/bin/libclang_rt.tsan_cxx-x86_64.a.syms"
        DESTINATION
          "lib/RMAOptimizerTSan/")
