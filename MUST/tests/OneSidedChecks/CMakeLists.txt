# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for communicator and group tracking tests.
#
# @author Tobias Hilbrich
# @date 06.03.2011
#

###################
#Add test case
FUNCTION (addOneSidedTest
        folder
        testName
        numProcs
        )

    add_compile_options("-O0" "-gdwarf-2" "-fno-omit-frame-pointer" "-fsanitize=thread")

    # Executable instrumented with TSan runtime
    addHybridTest ("${testName}.c.shadow.exe" "${folder}/${testName}.c" ${numProcs}
    "data race"
    ""
    "-O0 -gdwarf-2 -fno-omit-frame-pointer"
    "-Wl,--whole-archive ${CMAKE_BINARY_DIR}/lib/libonReportLoader.a -Wl,--no-whole-archive -ldl -fsanitize=thread"
    DEPENDS onReportLoader
    )

    # Executable instrumented with ISL runtime
    addHybridTest ("${testName}.c.isl.exe" "${folder}/${testName}.c" ${numProcs}
    "data race"
    ""
    "-O0 -gdwarf-2 -fno-omit-frame-pointer"
    "-Wl,--whole-archive ${CMAKE_BINARY_DIR}/lib/libtsanInterceptorStatic.a -Wl,--no-whole-archive -ldl"
    DEPENDS tsanInterceptorStatic
    )

ENDFUNCTION (addOneSidedTest)

IF(TEST_PREBUILDS)
	MUST_TEST_PREBUILD_CONFIGURATION(prebuild-OneSidedChecks-2Procs
	    "--must:distributed --must:rma \
             ${MPIEXEC_NUMPROC_FLAG} 2"
	    )
    MUST_TEST_PREBUILD_CONFIGURATION(prebuild-OneSidedChecks-3Procs
	    "--must:distributed --must:rma \
             ${MPIEXEC_NUMPROC_FLAG} 3"
	    )
ENDIF()

addOneSidedTest("AcrossProcesses" "APaccumulateDifferentDatatypesRace" 2)
addOneSidedTest("AcrossProcesses" "APaccumulateSameDatatypesSafe" 2)
addOneSidedTest("AcrossProcesses" "APactiveGetPutRace" 2)
addOneSidedTest("AcrossProcesses" "APactiveGetPutSafe" 2)
addOneSidedTest("AcrossProcesses" "APactiveLoadPutRace" 2)
addOneSidedTest("AcrossProcesses" "APactivePut3ProcsRace" 3)
addOneSidedTest("AcrossProcesses" "APactivePut3ProcsSafe" 3)
addOneSidedTest("AcrossProcesses" "APpassivePut3ProcsRace" 3)
addOneSidedTest("AcrossProcesses" "APpassivePut3ProcsSendRecvRace" 3)
addOneSidedTest("AcrossProcesses" "APpassivePut3ProcsSendRecvSafe" 3)
addOneSidedTest("AcrossProcesses" "APactiveStoreGetRace" 2)
addOneSidedTest("AcrossProcesses" "APactiveStoreGetSafe" 2)
addOneSidedTest("AcrossProcesses" "APconflictingBeforeLockRace" 2)
addOneSidedTest("AcrossProcesses" "APgeneralactiveGetPutRace" 3)
addOneSidedTest("AcrossProcesses" "APgeneralactiveGetPutSafe" 3)
addOneSidedTest("AcrossProcesses" "APpassiveLoadPutRace" 2)
addOneSidedTest("AcrossProcesses" "APpassiveLoadPutSafe" 2)
addOneSidedTest("AcrossProcesses" "APpassivePutLoadSafe" 2)
addOneSidedTest("AcrossProcesses" "APpassivePutPutRace" 2)
addOneSidedTest("AcrossProcesses" "APpassivePutPutSafe" 2)
addOneSidedTest("AcrossProcesses" "APpassivePutPutSameOriginRace" 2)
addOneSidedTest("AcrossProcesses" "APpassivePutPutSameTargetRace" 2)
addOneSidedTest("AcrossProcesses" "APpassiveStoreGetFlushLocalSafe" 2)
addOneSidedTest("AcrossProcesses" "APpassiveStorePutFlushLocalRace" 2)
addOneSidedTest("AcrossProcesses" "APpassiveStoreRgetSafe" 2)
addOneSidedTest("AcrossProcesses" "APpassiveStoreRputRace" 2)
addOneSidedTest("AcrossProcesses" "APPollingRace" 2)
addOneSidedTest("AcrossProcesses" "APwinLockSyncNotObservableRace" 2)
addOneSidedTest("AcrossProcesses" "APwinLockSyncRace" 2)


addOneSidedTest("ProcessLocal" "PLactiveGetPutRace" 2)
addOneSidedTest("ProcessLocal" "PLactiveGetPutSafe" 2)
addOneSidedTest("ProcessLocal" "PLactiveStorePutRace" 2)
addOneSidedTest("ProcessLocal" "PLactiveStorePutSafe" 2)
addOneSidedTest("ProcessLocal" "PLpassiveLoadGetRace" 2)
addOneSidedTest("ProcessLocal" "PLpassiveLoadGetSafe" 2)
addOneSidedTest("ProcessLocal" "PLpassivePutGetRace" 2)
addOneSidedTest("ProcessLocal" "PLpassivePutGetSafe" 2)
addOneSidedTest("ProcessLocal" "PLpassiveRgetRace" 2)
addOneSidedTest("ProcessLocal" "PLpassiveRgetSafe" 2)

addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_fence_Get_store_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_fence_Put_Get_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_fence_Put_Put_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_fence_Put_store_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_lock_all_Get_store_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_lock_all_Put_Get_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_lock_all_Put_Put_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_lock_all_Put_store_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_lock_Get_store_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_lock_Put_Get_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_lock_Put_Put_nok" 3)
addOneSidedTest("MPIBugsInitiative/GlobalConcurrency" "GlobalConcurrency_Win_lock_Put_store_nok" 3)

addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_Get_Get_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_Get_load_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_Get_loadstore_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_Get_Put_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_Get_store_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_load_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_loadstore_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_Put_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_Put_store_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_store_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_fence_store_Put_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_Get_Get_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_Get_load_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_Get_loadstore_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_Get_Put_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_Get_store_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_load_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_loadstore_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_Put_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_Put_store_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_store_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_all_store_Put_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_Get_Get_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_Get_load_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_Get_loadstore_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_Get_Put_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_Get_store_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_load_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_loadstore_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_Put_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_Put_store_nok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_store_Get_ok" 2)
addOneSidedTest("MPIBugsInitiative/LocalConcurrency" "MBI_LocalConcurrency_Win_lock_store_Put_ok" 2)
