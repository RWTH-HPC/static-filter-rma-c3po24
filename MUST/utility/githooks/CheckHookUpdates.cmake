cmake_minimum_required(VERSION 3.13.4...3.27.4)

function(get_git_hook_dir OUT_VARIABLE)
    execute_process(COMMAND git config core.hooksPath
            OUTPUT_VARIABLE HOOKS_PATH
            OUTPUT_STRIP_TRAILING_WHITESPACE
            )

    if ("${HOOKS_PATH}" STREQUAL "")
        set(HOOKS_PATH ".git/hooks")
    endif ()

    execute_process(COMMAND git rev-parse --show-toplevel
            OUTPUT_VARIABLE REPO_ROOT
            OUTPUT_STRIP_TRAILING_WHITESPACE
            )

    set(${OUT_VARIABLE} "${REPO_ROOT}/${HOOKS_PATH}" PARENT_SCOPE)
endfunction()

function(parse_version FILENAME OUT_VARIABLE)
    set(${OUT_VARIABLE} "" PARENT_SCOPE)
    file(STRINGS "${FILENAME}" LINES ENCODING UTF-8)
    foreach (line ${LINES})
        string(REGEX MATCH "^__version__[ \t\r\n]*=[ \t\r\n]*[\"']([0-9]+\\.[0-9]+\\.[0-9]+)[\"']" RESULT "${line}")
        if (NOT "${RESULT}" STREQUAL "")
            set(${OUT_VARIABLE} ${CMAKE_MATCH_1} PARENT_SCOPE)
        endif ()
    endforeach ()
endfunction()

function(is_hook_outdated hook RESULT)
    set(${RESULT} False PARENT_SCOPE)

    set(REPO_HOOK_DIR ${CMAKE_CURRENT_LIST_DIR})
    get_git_hook_dir(GIT_HOOKS_PATH)

    if (NOT EXISTS ${GIT_HOOKS_PATH}/${hook})
        return()
    endif ()

    parse_version(${GIT_HOOKS_PATH}/${hook} GIT_HOOK_VERSION)
    parse_version(${REPO_HOOK_DIR}/${hook} REPO_HOOK_VERSION)

    if (GIT_HOOK_VERSION VERSION_LESS REPO_HOOK_VERSION)
        set(${RESULT} True PARENT_SCOPE)
    endif ()
endfunction()

function(check_hooks)
    foreach (hook ${ARGN})
        is_hook_outdated(${hook} OUTDATED)
        if (OUTDATED)
            message(AUTHOR_WARNING "The git hook \"${hook}\" is outdated.")
        endif ()
    endforeach ()
endfunction()

check_hooks(commit-msg pre-commit)
