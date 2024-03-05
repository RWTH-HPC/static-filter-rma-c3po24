if (COMPILER_RT_BUILD_SANITIZERS)
  set(SANITIZER_HEADERS
    sanitizer/allocator_interface.h
    sanitizer/asan_interface.h
    sanitizer/common_interface_defs.h
    sanitizer/coverage_interface.h
    sanitizer/dfsan_interface.h
    sanitizer/hwasan_interface.h
    sanitizer/linux_syscall_hooks.h
    sanitizer/lsan_interface.h
    sanitizer/msan_interface.h
    sanitizer/netbsd_syscall_hooks.h
    sanitizer/scudo_interface.h
    sanitizer/tsan_interface.h
    sanitizer/tsan_interface_atomic.h
    sanitizer/ubsan_interface.h
    )
  set(FUZZER_HEADERS
    fuzzer/FuzzedDataProvider.h
    )
endif(COMPILER_RT_BUILD_SANITIZERS)

if (COMPILER_RT_BUILD_MEMPROF)
  set(MEMPROF_HEADERS
    sanitizer/memprof_interface.h
    profile/MemProfData.inc
    )
endif(COMPILER_RT_BUILD_MEMPROF)

if (COMPILER_RT_BUILD_XRAY)
  set(XRAY_HEADERS
    xray/xray_interface.h
    xray/xray_log_interface.h
    xray/xray_records.h
    )
endif(COMPILER_RT_BUILD_XRAY)

if (COMPILER_RT_BUILD_ORC)
  set(ORC_HEADERS
    orc_rt/c_api.h
    )
endif(COMPILER_RT_BUILD_ORC)

if (COMPILER_RT_BUILD_PROFILE)
  set(PROFILE_HEADERS
    profile/InstrProfData.inc
    )
endif(COMPILER_RT_BUILD_PROFILE)

set(COMPILER_RT_HEADERS
  ${SANITIZER_HEADERS}
  ${FUZZER_HEADERS}
  ${MEMPROF_HEADERS}
  ${XRAY_HEADERS}
  ${ORC_HEADERS}
  ${PROFILE_HEADERS})

set(output_dir ${COMPILER_RT_OUTPUT_DIR}/include)

# Copy compiler-rt headers to the build tree.
set(out_files)
foreach( f ${COMPILER_RT_HEADERS} )
  set( src ${CMAKE_CURRENT_SOURCE_DIR}/${f} )
  set( dst ${output_dir}/${f} )
  add_custom_command(OUTPUT ${dst}
    DEPENDS ${src}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${src} ${dst}
    COMMENT "Copying compiler-rt's ${f}...")
  list(APPEND out_files ${dst})
endforeach( f )

add_custom_target(compiler-rt-headers ALL DEPENDS ${out_files})
add_dependencies(compiler-rt compiler-rt-headers)
set_target_properties(compiler-rt-headers PROPERTIES FOLDER "Compiler-RT Misc")
