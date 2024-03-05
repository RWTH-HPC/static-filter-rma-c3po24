function(must_target_add_sanitizer target sanitizer)
    set(isUbsanAllowed "$<NOT:$<BOOL:$<TARGET_PROPERTY:MUST_NO_UBSAN>>>")
    set(isClang "$<OR:$<C_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:Clang>>")
    if (CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
        set(isGfortran "$<COMPILE_LANGUAGE:Fortran>")
    else ()
        set(isGfortran "0")
    endif ()

    set(sharedLibsanExpr "$<$<AND:${isUbsanAllowed},${isClang},$<NOT:${isGfortran}>>:-shared-libsan>")

    target_compile_options(${target} PRIVATE
            "$<${isUbsanAllowed}:-fsanitize=${sanitizer}>"
            -fno-omit-frame-pointer
            "${sharedLibsanExpr}"
            )
    target_link_options(${target} PRIVATE
            "$<${isUbsanAllowed}:-fsanitize=${sanitizer}>"
            "${sharedLibsanExpr}"
            )
endfunction()

function(must_target_add_ubsan target)
    must_target_add_sanitizer(${target} undefined)
endfunction()

function(must_target_add_asan target)
    must_target_add_sanitizer(${target} address)
endfunction()
