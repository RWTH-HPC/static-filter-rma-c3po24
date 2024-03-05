IF (NOT HAVE_TSAN OR NOT ENABLE_TSAN)
    RETURN()
ENDIF ()

IF(TEST_PREBUILDS)
    MUST_TEST_PREBUILD_CONFIGURATION(
        prebuild-MpiTsan
        "--must:tsan \
         ${MPIEXEC_NUMPROC_FLAG} 2"
        )
    MUST_TEST_PREBUILD_CONFIGURATION(
        prebuild-MpiTsan-distributed
        "--must:tsan --must:distributed \
         ${MPIEXEC_NUMPROC_FLAG} 2"
        )
ENDIF()

add_compile_options("-g" "-fsanitize=thread")
set (linkerFlags "-Wl,--whole-archive ${CMAKE_BINARY_DIR}/lib/libonReportLoader.a -Wl,--no-whole-archive -ldl -fsanitize=thread")

addHybridTest ("MpiTSanBcast" "bcast.c" 2
    "data race"
    ""
    "-fsanitize=thread"
    ${linkerFlags}
    DEPENDS onReportLoader
    )

addHybridTest ("MpiTSanIbcast" "ibcast.c" 2
    "data race"
    ""
    "-fsanitize=thread"
    ${linkerFlags}
    DEPENDS onReportLoader
    )

addHybridTest ("MpiTSanRecv" "recv.c" 2
    "data race"
    ""
    "-fsanitize=thread"
    ${linkerFlags}
    DEPENDS onReportLoader
    )

addHybridTest ("MpiTSanIrecv" "irecv.c" 2
    "data race"
    ""
    "-fsanitize=thread"
    ${linkerFlags}
    DEPENDS onReportLoader
    )

addHybridTest ("MpiTSanScatter" "scatter.c" 2
    "data race"
    ""
    "-fsanitize=thread"
    ${linkerFlags}
    DEPENDS onReportLoader
    )

addHybridTest ("MpiTSanIscatter" "iscatter.c" 2
    "data race"
    ""
    "-fsanitize=thread"
    ${linkerFlags}
    DEPENDS onReportLoader
    )

addHybridTest ("MpiTSanIsendSingle" "isend_single.c" 2
    "data race"
    ""
    "-fsanitize=thread"
    ${linkerFlags}
    DEPENDS onReportLoader
    )

addHybridTest ("MpiTSanIsend" "isend.c" 2
    "data race"
    ""
    "-fsanitize=thread"
    ${linkerFlags}
    DEPENDS onReportLoader
    )

addHybridTest ("MpiTSanSend" "send.c" 2 
    "data race"
    ""
    "-fsanitize=thread"
    ${linkerFlags}
    DEPENDS onReportLoader
    )
