# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause
#
# @file MTHelperMacros.cmake
#       Helpful CMake macros for GTI mini tools.
#
# @author Tobias Hilbrich
# @date 03.01.2011


SET(MUSTRUN_TIMEOUT 40)

####################################################
## Macro configureTest
##
## Creates a layout specification for a simple test
## case that uses three layers and GTI's MPI based 
## communication.
##
## Layer 0 and layer 1 can be specified in their size, whereas layer 2
## always has exactly 1 process.
## Lists of module names are used to specify what modules run on 
## which levels.
####################################################
MACRO (
    configureTest 
        outLayoutSpecName  #Filepath of layout specification to generate
        l0Mods                      #List of modules on layer 0, format: "<Mod0Name>:<Mod0Group>;<Mod1Name>:<Mod1Group>;..."
        l1Mods                      #List of modules on layer 1
        l2Mods                      #List of modules on layer 2
        numProcsL0               #Number of processes used for process 0
        numProcsL1               #Number of processes used for process 1
        )

        ##Debug Out
        #MESSAGE ("outRunScriptName=${outRunScriptName} outLayoutSpecName=${outLayoutSpecName} l0Mods=${l0Mods} l1Mods=${l1Mods} l2Mods=${l2Mods} numProcsL0=${numProcsL0} numProcsL1=${numProcsL1} extraApiSpecs=${extraApiSpecs} extraAnalysisSpecs=${extraAnalysisSpecs} executableName=${executableName}")
        
        SET (outLayoutSpecName "${outLayoutSpecName}")
        SET (l0Mods "${l0Mods}")
        SET (l1Mods "${l1Mods}")
        SET (l2Mods "${l2Mods}")
        SET (numProcsL0 ${numProcsL0})
        SET (numProcsL1 ${numProcsL1})

        #Calculate important variables for the run script
        MATH (EXPR totalProcs  "${numProcsL0} + ${numProcsL1} + 1")
        
        #Calculate variables for layout spec
        SET(levels "0" "1" "2")
        FOREACH (level ${levels})
            SET (level${level}ModString "")
            FOREACH (module ${l${level}Mods})
                STRING (REGEX MATCH "^[^:]*" modName ${module})
                STRING (REGEX MATCH "[^:]*$" modGroup ${module})
            
                SET (level${level}ModString "${level${level}ModString} \n<analysis name=\"${modName}\" group=\"${modGroup}\"></analysis>")    
            ENDFOREACH (module)
        ENDFOREACH (level)
        
        #Configure the layout specification
        CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/cmakemodules/layout.template.xml ${outLayoutSpecName})
ENDMACRO (configureTest)

####################################################
## Macro configureTestIntra
##
## Creates a layout specification for a simple test
## case that uses three layers and GTI's MPI based 
## communication. Difference to configureTest is 
## that layer 1 provides intra layer communication.
##
## Layer 0 and layer 1 can be specified in their size, whereas layer 2
## always has exactly 1 process.
## Lists of module names are used to specify what modules run on 
## which levels.
####################################################
MACRO (
    configureTestIntra
        outLayoutSpecName  #Filepath of layout specification to generate
        l0Mods                      #List of modules on layer 0, format: "<Mod0Name>:<Mod0Group>;<Mod1Name>:<Mod1Group>;..."
        l1Mods                      #List of modules on layer 1
        l2Mods                      #List of modules on layer 2
        numProcsL0               #Number of processes used for process 0
        numProcsL1               #Number of processes used for process 1
        )

        ##Debug Out
        #MESSAGE ("outRunScriptName=${outRunScriptName} outLayoutSpecName=${outLayoutSpecName} l0Mods=${l0Mods} l1Mods=${l1Mods} l2Mods=${l2Mods} numProcsL0=${numProcsL0} numProcsL1=${numProcsL1} extraApiSpecs=${extraApiSpecs} extraAnalysisSpecs=${extraAnalysisSpecs} executableName=${executableName}")
        
        SET (outLayoutSpecName "${outLayoutSpecName}")
        SET (l0Mods "${l0Mods}")
        SET (l1Mods "${l1Mods}")
        SET (l2Mods "${l2Mods}")
        SET (numProcsL0 ${numProcsL0})
        SET (numProcsL1 ${numProcsL1})

        #Calculate important variables for the run script
        MATH (EXPR totalProcs  "${numProcsL0} + ${numProcsL1} + 1")
        
        #Calculate variables for layout spec
        SET(levels "0" "1" "2")
        FOREACH (level ${levels})
            SET (level${level}ModString "")
            FOREACH (module ${l${level}Mods})
                STRING (REGEX MATCH "^[^:]*" modName ${module})
                STRING (REGEX MATCH "[^:]*$" modGroup ${module})
            
                SET (level${level}ModString "${level${level}ModString} \n<analysis name=\"${modName}\" group=\"${modGroup}\"></analysis>")    
            ENDFOREACH (module)
        ENDFOREACH (level)
        
        #Configure the layout specification
        CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/cmakemodules/layoutIntra.template.xml ${outLayoutSpecName})
ENDMACRO (configureTestIntra)

####################################################
## Macro addTestExecutable
##
## Creates an executable target for a test MPI application using 
## the PnMPI library as dependency.
####################################################
MACRO (addTestExecutable targetName sourceList)
    #Set libs, dirs, ... for executable
    SET(CMAKE_C_FLAGS ${MPI_C_COMPILE_FLAGS})
    SET(CMAKE_CXX_FLAGS ${MPI_CXX_COMPILE_FLAGS})
    SET(CMAKE_Fortran_FLAGS ${MPI_Fortran_COMPILE_FLAGS})
    INCLUDE_DIRECTORIES (${MPI_C_INCLUDE_PATH} ${MPI_CXX_INCLUDE_PATH} ${MPI_Fortran_INCLUDE_PATH})
    
     #Determine the compilers to use
    FOREACH (lan C CXX Fortran)
        SET (${lan}_compiler_to_use ${CMAKE_${lan}_COMPILER})
        IF (MPI_${lan}_COMPILER)
            SET (${lan}_compiler_to_use ${MPI_${lan}_COMPILER})
        ENDIF ()
    ENDFOREACH ()
    
    SET(CMAKE_C_COMPILER ${C_compiler_to_use})
    SET(CMAKE_CXX_COMPILER ${CXX_compiler_to_use})
    SET(CMAKE_Fortran_COMPILER ${Fortran_compiler_to_use})
    SET(CMAKE_EXE_LINKER_FLAGS ${MPI_C_LINK_FLAGS})
    
    #Is this a fortran test?
    LIST(GET ${sourceList} 1 firstSource)
    IF (NOT firstSource)
        SET (firstSource ${sourceList})
    ENDIF ()
    IF( firstSource MATCHES "[.]f$$" OR
        firstSource MATCHES "[.]F$$" OR
        firstSource MATCHES "[.]f77$$" OR
        firstSource MATCHES "[.]F77$$" OR
        firstSource MATCHES "[.]f90$$" OR
        firstSource MATCHES "[.]F90$$" 
       )
        SET (TEST_LIBRARIES ${PnMPI_Fortran_LIBRARIES})
    ELSE ()
        SET (TEST_LIBRARIES ${PnMPI_C_LIBRARIES})
    ENDIF () 
    
    #Add test case
    ADD_EXECUTABLE (${targetName} ${sourceList})
    TARGET_LINK_LIBRARIES(${targetName} ${TEST_LIBRARIES})

    if (TARGET FileCheck_Standalone AND MUST_BUILD_FILECHECK)
        add_dependencies(${targetName} FileCheck_Standalone)
    endif()
    if (TARGET prebuilds)
        add_dependencies(${targetName} prebuilds)
    endif()
    if (CURRENT_TEST_SUITE)
        add_dependencies(build-test-${CURRENT_TEST_SUITE} ${targetName})
    endif()
ENDMACRO (addTestExecutable)

####################################################
## Macro addProcessLocalTest
##
## Creates a test executable and applies a certain set of modules
## to it. The modules are run on tha application process, logging
## is done on the application via std out logging.
####################################################
MACRO (addProcessLocalTest
    testName    # name used for this test (must be unique)
                       # ${testName}Test is used as the name for the CMake test 
    sourceList   # sources needed for the test executable
    numProcs   # number of application processes, all other layers have 1 process and no modules
    passRegexp # Regular  expression needed to pass the test, only considered it not ""
    failRegexp  # Regular  expression that causes the test to fail, only considered it not ""
    )

    #Create the executable for the test
    addTestExecutable (${testName} "${sourceList}")
    
    #Add the trigger to run the script
    ADD_TEST(NAME ${testName}Test COMMAND
        bash ${MUSTRUN} --must:timeout ${MUSTRUN_TIMEOUT} 
                        --must:output stdout
                        --must:mpiexec ${MPIEXEC}
                        --must:np ${MPIEXEC_NUMPROC_FLAG}  
                        --must:temp temp${testName}
                        --must:clean 
                        ${MPIEXEC_NUMPROC_FLAG} ${numProcs}  $<TARGET_FILE:${testName}> ;
        )

    #Add a regular eypression for succes if specified    
    IF (NOT "${passRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${testName}Test PROPERTIES PASS_REGULAR_EXPRESSION "${passRegexp}")
    ENDIF (NOT "${passRegexp}" STREQUAL "")
    
    IF (NOT "${failRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${testName}Test PROPERTIES FAIL_REGULAR_EXPRESSION "${failRegexp}")
    ENDIF (NOT "${failRegexp}" STREQUAL "")
ENDMACRO (addProcessLocalTest)

####################################################
## Macro addProcessLocalTest
##
## Creates a test executable and applies a certain set of modules
## to it. The modules are run on tha application process, logging
## is done on the application via std out logging.
####################################################
MACRO (addHybridTest
    testName    # name used for this test (must be unique)
                       # ${testName}Test is used as the name for the CMake test 
    sourceList   # sources needed for the test executable
    numProcs   # number of application processes, all other layers have 1 process and no modules
    passRegexp # Regular  expression needed to pass the test, only considered it not ""
    failRegexp  # Regular  expression that causes the test to fail, only considered it not ""
    compilerFlags # additional flags used for compiling the test
    linkerItems
    )

    IF (NOT OpenMP_FOUND)
        RETURN()
    ENDIF ()

    cmake_parse_arguments(MY_ARGS "" "" "DEPENDS" ${ARGN})
    #Create the executable for the test
    set(MPI_C_COMPILE_FLAGS "${MPI_C_COMPILE_FLAGS} ${OpenMP_C_FLAGS}")
    set(MPI_CXX_COMPILE_FLAGS "${MPI_CXX_COMPILE_FLAGS} ${OpenMP_CXX_FLAGS}")

#    ADD_COMPILE_OPTIONS()
    addTestExecutable (${testName} "${sourceList}")
    if(MY_ARGS_DEPENDS)
	add_dependencies(${testName} ${MY_ARGS_DEPENDS})
    endif()
    TARGET_LINK_LIBRARIES(${testName} ${linkerItems})

    #Add the trigger to run the script
    ADD_TEST(NAME ${testName}Test COMMAND
        bash ${MUSTRUN} --must:timeout ${MUSTRUN_TIMEOUT} 
                        --must:output stdout
                        --must:mpiexec ${MPIEXEC}
                        --must:np ${MPIEXEC_NUMPROC_FLAG}  
                        --must:temp temp${testName}
                        --must:clean 
                  		--must:hybrid
                        ${MPIEXEC_NUMPROC_FLAG} ${numProcs}  $<TARGET_FILE:${testName}> ;
        )

    #Add a regular eypression for succes if specified    
    IF (NOT "${passRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${testName}Test PROPERTIES PASS_REGULAR_EXPRESSION "${passRegexp}")
    ENDIF (NOT "${passRegexp}" STREQUAL "")
    
    IF (NOT "${failRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${testName}Test PROPERTIES FAIL_REGULAR_EXPRESSION "${failRegexp}")
    ENDIF (NOT "${failRegexp}" STREQUAL "")
ENDMACRO (addHybridTest)

####################################################
## Macro addDDlTest
##
## Creates a test executable and configuresit such that MUST will
## apply distributed deadlock detection to it.
####################################################
MACRO (addDDlTest
    testName    # name used for this test (must be unique)
                       # ${testName}Test is used as the name for the CMake test 
    sourceList   # sources needed for the test executable
    numProcs   # number of application processes, all other layers have 1 process and no modules
    passRegexp # Regular  expression needed to pass the test, only considered it not ""
    failRegexp  # Regular  expression that causes the test to fail, only considered it not ""
    )

    #Create the executable for the test
    addTestExecutable (${testName} "${sourceList}")
    
    #Add the trigger to run the script
    ADD_TEST(NAME ${testName}Test COMMAND
        bash ${MUSTRUN} --must:timeout ${MUSTRUN_TIMEOUT} 
                        --must:output stdout
                        --must:mpiexec ${MPIEXEC}
                        --must:np ${MPIEXEC_NUMPROC_FLAG}  
                        --must:temp temp${testName}
                        --must:distributed
                        --must:dl
                        --must:fanin 2
                        --must:clean 
                        ${MPIEXEC_NUMPROC_FLAG} ${numProcs}  $<TARGET_FILE:${testName}> ;
        )

    #Add a regular eypression for succes if specified    
    IF (NOT "${passRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${testName}Test PROPERTIES PASS_REGULAR_EXPRESSION "${passRegexp}")
    ENDIF (NOT "${passRegexp}" STREQUAL "")
    
    IF (NOT "${failRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${testName}Test PROPERTIES FAIL_REGULAR_EXPRESSION "${failRegexp}")
    ENDIF (NOT "${failRegexp}" STREQUAL "")
ENDMACRO (addDDlTest)

####################################################
## Macro addProcessLocalTestWithAnalysis
##
## Creates a test executable and applies a certain set of modules
## to it. The modules are run on tha application process, logging
## is done on the application via std out logging.
## -->   as above + analysis_spec
####################################################
MACRO (addProcessLocalTestWithAnalysis
    testName    # name used for this test (must be unique)
                       # ${testName}Test is used as the name for the CMake test 
    sourceList   # sources needed for the test executable
    numProcs   # number of application processes, all other layers have 1 process and no modules
    modules     # modules to run on the application, note that logging is automatically added, syntax as in the configureTest macro
    analysis_spec # specification of analysis
    passRegexp # Regular  expression needed to pass the test, only considered it not ""
    failRegexp  # Regular  expression that causes the test to fail, only considered it not ""
    )

    #Create the executable for the test
    addTestExecutable (${testName} "${sourceList}")
    
    #Configure the scripts for the test
    configureTest (
        ${CMAKE_CURRENT_BINARY_DIR}/${testName}Layout.xml
        "${modules}"
        ""
        "MsgLoggerStdOut:MUST_Base"
        ${numProcs}
        1
        )

    IF(TEST_PREBUILDS)
	MUST_PREBUILD_CONFIGURATION(
	    prebuild-${testName}
	    "--must:layout ${CMAKE_CURRENT_BINARY_DIR}/${testName}Layout.xml \
             --must:analyses ${analysis_spec} \
             ${MPIEXEC_NUMPROC_FLAG} ${numProcs}"
            DEST ${CMAKE_BINARY_DIR}/tests/prebuilds
            EXCLUDE_FROM_PREBUILDS
	    )
    ENDIF()
    
   #Add the trigger to run the script
    ADD_TEST(NAME ${testName}Test COMMAND
        bash ${MUSTRUN} --must:timeout ${MUSTRUN_TIMEOUT} --must:mpiexec ${MPIEXEC} --must:np ${MPIEXEC_NUMPROC_FLAG}  --must:layout ${CMAKE_CURRENT_BINARY_DIR}/${testName}Layout.xml --must:analyses ${analysis_spec} --must:temp temp${testName} --must:clean ${MPIEXEC_NUMPROC_FLAG} ${numProcs}   "$<TARGET_FILE:${testName}>" 
        )
        
    #Add a regular eypression for succes if specified    
    IF (NOT "${passRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${testName}Test PROPERTIES PASS_REGULAR_EXPRESSION "${passRegexp}")
    ENDIF (NOT "${passRegexp}" STREQUAL "")
    
    IF (NOT "${failRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${testName}Test PROPERTIES FAIL_REGULAR_EXPRESSION "${failRegexp}")
    ENDIF (NOT "${failRegexp}" STREQUAL "")
ENDMACRO (addProcessLocalTestWithAnalysis)

#=========================================================
# Macro mergeLists
# Does:
## Merges two lists (list1, list2) and put them into list ret.
# 
#=========================================================
MACRO (mergeLists
    ret
    list1 
    list2) 
    
    SET (res ${${list1}})
    FOREACH(item ${${list2}})      
    STRING (REPLACE " " ";" mItem ${item})
    FOREACH(nItem ${mItem})
            SET(avoid FALSE)
            FOREACH( sitem ${res})
                IF(${sitem} STREQUAL ${nItem})
                    SET (avoid TRUE)
                ENDIF ()
            ENDFOREACH()
            IF(NOT avoid)
                SET(res ${res} ${nItem})
            ENDIF ()
    ENDFOREACH ()
    ENDFOREACH ()
    SET (${ret} ${res})
ENDMACRO (mergeLists)

#=========================================================
# Macro addProcessLocalTestMixed 
# Does:
## Creates a test executable and applies a certain set of modules
## to it. The modules are run on tha application process, logging
## is done on the application via std out logging.
# 
# language: in C, CXX, Fortran
#=========================================================
MACRO (addProcessLocalTestMixed 
    targetName    # name used for this test (must be unique)
                       # ${testName}Test is used as the name for the CMake test
    primarySource     # source that uses secondary source 
    secondarySource   # source that is used by primary source
    numProcs   # number of application processes, all other layers have 1 process and no modules
    language   # use C if it is c, otherwise fortran
    modules     # modules to run on the application, note that logging is automatically added, syntax as in the configureTest macro
    passRegexp # Regular  expression needed to pass the test, only considered it not ""
    failRegexp  # Regular  expression that causes the test to fail, only considered it not "
    )
   
    #Set libs, dirs, ... for executable
    SET(CMAKE_C_FLAGS ${MPI_C_COMPILE_FLAGS})
    SET(CMAKE_Fortran_FLAGS ${MPI_Fortran_COMPILE_FLAGS})
    INCLUDE_DIRECTORIES (${MPI_C_INCLUDE_PATH} ${MPI_Fortran_INCLUDE_PATH})
    
     #Determine the compilers to use
    FOREACH (lan C Fortran)
        SET (${lan}_compiler_to_use ${CMAKE_${lan}_COMPILER})
        IF (MPI_${lan}_COMPILER)
            SET (${lan}_compiler_to_use ${MPI_${lan}_COMPILER})
        ENDIF ()
    ENDFOREACH ()
    
    SET(CMAKE_C_COMPILER ${C_compiler_to_use})
    SET(CMAKE_Fortran_COMPILER ${Fortran_compiler_to_use})
    SET(CMAKE_EXE_LINKER_FLAGS ${MPI_C_LINK_FLAGS})
    
    ##Add fortran Libraries
    mergeLists(ret MPI_C_LIBRARIES MPI_Fortran_LIBRARIES)
    
    IF (${language} STREQUAL "C")
        SET (TEST_LIBRARIES ${PnMPI_C_LIBRARIES})
    ELSE ()
        SET (TEST_LIBRARIES ${PnMPI_Fortran_LIBRARIES})
    ENDIF () 
       
    #Add test case
    ADD_LIBRARY(l${targetName} STATIC ${secondarySource})
    ADD_EXECUTABLE (${targetName} ${primarySource})
    TARGET_LINK_LIBRARIES(${targetName} l${targetName} ${TEST_LIBRARIES} ${ret})
    IF (${language} STREQUAL "C")
        TARGET_LINK_LIBRARIES(${targetName} ${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES})
        SET_TARGET_PROPERTIES(${targetName} PROPERTIES LINKER_LANGUAGE C)
    ELSE ()
        TARGET_LINK_LIBRARIES(${targetName} ${CMAKE_CXX_IMPLICIT_LINK_LIBRARIES})
        SET_TARGET_PROPERTIES(${targetName} PROPERTIES LINKER_LANGUAGE Fortran)
    ENDIF()
    
    ## ADD THE TEST
    #Configure the scripts for the test
    configureTest (
        ${CMAKE_CURRENT_BINARY_DIR}/${targetName}Layout.xml
        "${modules}"
        ""
        "MsgLoggerStdOut:MUST_Base"
        ${numProcs}
        1
        )

    IF(TEST_PREBUILDS)
	MUST_PREBUILD_CONFIGURATION(
	    prebuild-${targetName}
	    "--must:layout ${CMAKE_CURRENT_BINARY_DIR}/${targetName}Layout.xml \
            ${MPIEXEC_NUMPROC_FLAG} ${numProcs}"
            DEST ${CMAKE_BINARY_DIR}/tests/prebuilds
            EXCLUDE_FROM_PREBUILDS
	    )
    ENDIF()
    
    #Add the trigger to run the script
    ADD_TEST(NAME ${targetName}Test COMMAND
        bash ${MUSTRUN} --must:timeout ${MUSTRUN_TIMEOUT} --must:mpiexec ${MPIEXEC} --must:np ${MPIEXEC_NUMPROC_FLAG}  --must:layout ${CMAKE_CURRENT_BINARY_DIR}/${targetName}Layout.xml --must:temp temp${targetName} --must:clean ${MPIEXEC_NUMPROC_FLAG} ${numProcs}   $<TARGET_FILE:${targetName}> 
        )

    #Add a regular eypression for succes if specified    
    IF (NOT "${passRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${targetName}Test PROPERTIES PASS_REGULAR_EXPRESSION "${passRegexp}")
    ENDIF (NOT "${passRegexp}" STREQUAL "")
    
    IF (NOT "${failRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${targetName}Test PROPERTIES FAIL_REGULAR_EXPRESSION "${failRegexp}")
    ENDIF (NOT "${failRegexp}" STREQUAL "")

    if (CURRENT_TEST_SUITE)
        add_dependencies(build-test-${CURRENT_TEST_SUITE} ${targetName})
    endif()
ENDMACRO (addProcessLocalTestMixed)

########################################################################
#
# Creates a check target that executes lit test suites.
#
# Signature:
# ```
# must_add_check_target(
#   SUITES lit_test_path1 [lit_test_path2 ...]
#   [NAME target_name]
#   [COMMENT comment]
#   [FAST_TESTS]
#   )
# ```
#
# The options are:
# - `SUITES`: Required. One or more paths of lit test suites.
# - `NAME`: The name of the target. Defaults to `check-<lit_test_path1>`.
# - `COMMENT`: The comment displayed by the target.
# - `FAST_TESTS`: Selects only fast tests by passing the environment
#   variable `MUST_FAST_TESTS_ONLY` to llvm-lit.
#
########################################################################
function(must_add_check_target)
    # Let CMakes's parser do the heavy lifting
    set(options FAST_TESTS)
    set(oneValueArgs NAME COMMENT)
    set(multiValueArgs SUITES DEPENDS)
    cmake_parse_arguments(PARSE_ARGV 0 ADD_CHECK_TARGET "${options}" "${oneValueArgs}" "${multiValueArgs}")

    # Again some input validation. Devs cannot be trusted.
    list(LENGTH ADD_CHECK_TARGET_SUITES suiteCount)
    if(suiteCount EQUAL 0)
        message(FATAL_ERROR "You have to pass at least one lit test suite path.")
    endif()

    # Process the optional target name argument
    if(ADD_CHECK_TARGET_NAME)
        set(target-name ${ADD_CHECK_TARGET_NAME})
    else()
        list(GET ADD_CHECK_TARGET_SUITES 0 firstSuite)
        set(target-name check-${firstSuite})
    endif()

    if(NOT ADD_CHECK_TARGET_COMMENT)
        list(GET ADD_CHECK_TARGET_SUITES 0 firstSuite)
        set(ADD_CHECK_TARGET_COMMENT "Running test suites: MUST::${firstSuite}")
        # Get the lists tail
        set(tmpList ${ADD_CHECK_TARGET_SUITES})
        list(REMOVE_AT tmpList 0)
        foreach(suite IN LISTS tmpList)
            set(ADD_CHECK_TARGET_COMMENT "${ADD_CHECK_TARGET_COMMENT}, MUST::${suite}")
        endforeach()
    endif()

    set(envVars "INTERNAL_MUST_PREBUILD_PATH=${CMAKE_BINARY_DIR}/tests/prebuilds")

    # Process flags
    if(ADD_CHECK_TARGET_FAST_TESTS)
        set(envVars "${envVars} MUST_FAST_TESTS_ONLY=1")
    endif()

    # Prepare escaped paths (who doesn't love spaces in paths?)
    foreach(suite_path IN LISTS ADD_CHECK_TARGET_SUITES)
        set(suitePaths ${suitePaths} "${suite_path}")
    endforeach()
    add_custom_target("${target-name}"
            COMMAND ${CMAKE_COMMAND} -E env ${envVars}
            "${MUST_LIT_EXECUTABLE}"
            --output "${target-name}.out.json"
            --xunit-xml-output "${target-name}.xunit.xml"
            --verbose
            --workers ${TESTS_WORKERS}
            --time-tests
            ${suitePaths}
            VERBATIM
            BYPRODUCTS ${target-name}.out.json ${target-name}.xunit.xml
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "${ADD_CHECK_TARGET_COMMENT}"
            USES_TERMINAL)
    if (ADD_CHECK_TARGET_DEPENDS)
        add_dependencies(${target-name} ${ADD_CHECK_TARGET_DEPENDS})
    endif()
endfunction()

macro(must_add_test_module)
    GTI_MAC_ADD_MODULE_NO_COVERAGE (${ARGN})
    add_dependencies(build-test-${CURRENT_TEST_SUITE} ${ARGV0})
endmacro()

MACRO (MUST_INSTALL)
  cmake_parse_arguments(MUST_INSTALL "" "DESTINATION" "FILES" ${ARGN} )
  FOREACH ( file ${MUST_INSTALL_FILES} ) 
    GET_FILENAME_COMPONENT( filename ${file} NAME )
    IF (NOT IS_ABSOLUTE ${file})
	set(file ${CMAKE_CURRENT_SOURCE_DIR}/${file})
    ENDIF()
    FILE ( GENERATE 
	OUTPUT ${CMAKE_BINARY_DIR}/${MUST_INSTALL_DESTINATION}/${filename} 
	INPUT ${file}
	)
  ENDFOREACH ( file )
  INSTALL ( FILES ${MUST_INSTALL_FILES} DESTINATION ${MUST_INSTALL_DESTINATION} )
ENDMACRO (MUST_INSTALL)

function(make_mpi_target target)
   target_link_libraries(${target} PRIVATE MPI::MPI_C)
endfunction()

macro(must_pythonize_bool var)
    if (${var})
        set(${var} True)
    else()
        set(${var} False)
    endif()
endmacro()
