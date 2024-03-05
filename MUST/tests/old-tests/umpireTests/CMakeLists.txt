# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause


# @file CMakeLists.cmake
#       CMake file for DatatypeTrack test.
#
# @author Tobias Hilbrich
# @date 23.02.2011
#

IF (DISABLE_UMPIRE_TESTS)
    RETURN()
ENDIF ()


####################################################
## Macro addProcessLocalUmpireTest
##
## Creates a test executable and applies a certain set of modules
## to it. The modules are run on tha application process, logging
## is done on the application via std out logging.
####################################################

MACRO (addProcessLocalUmpireTest
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
        bash ${MUSTRUN} 
            --must:timeout ${MUSTRUN_TIMEOUT}
            --must:output stdout
            --must:mpiexec ${MPIEXEC} 
            --must:np ${MPIEXEC_NUMPROC_FLAG}  
            --must:clean
            --must:temp temp${testName} ${MPIEXEC_NUMPROC_FLAG} ${numProcs} 
             $<TARGET_FILE:${testName}> 
        )
    #Add a regular eypression for succes if specified    
    IF (NOT "${passRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${testName}Test PROPERTIES PASS_REGULAR_EXPRESSION "${passRegexp}")
    ENDIF (NOT "${passRegexp}" STREQUAL "")
    
    IF (NOT "${failRegexp}" STREQUAL "")
        SET_TESTS_PROPERTIES (${testName}Test PROPERTIES FAIL_REGULAR_EXPRESSION "${failRegexp}")
    ENDIF (NOT "${failRegexp}" STREQUAL "")
ENDMACRO (addProcessLocalUmpireTest)


##################
#Recurse into sub directories
##Not needed

###################
#Add test case

## abort 
addProcessLocalUmpireTest ("umpire_abort" "abort.c" 2 
   "Error: from: call MPI_Abort@.*[0-9]: Argument 2 [(]errorcode[)] has to be a non-negative integer, but is negative [(]errorcode=-1[)]!"
   "")

## abort1 
addProcessLocalUmpireTest ("umpire_abort1" "abort1.c" 2 
   "Error: from: call MPI_Abort@.*[1-9]: Argument 2 [(]errorcode[)] has to be a non-negative integer, but is negative"
   "Error: from: call MPI_Abort@.*0: Argument 2 [(]errorcode[)] has to be a non-negative integer, but is negative")


## abort2 
addProcessLocalUmpireTest ("umpire_abort2" "abort2.c" 2 
   "Error: from: call MPI_Abort@.*0: Argument 2 [(]errorcode[)] has to be a non-negative integer, but is negative"
   "")


## abort3 
addProcessLocalUmpireTest ("umpire_abort3" "abort3.c" 3 
   "Error: from: call MPI_Abort@.*1: Argument 2 [(]errorcode[)] has to be a non-negative integer, but is negative"
   "Error: from: call MPI_Abort@.*[0,2-9]: Argument 2 [(]errorcode[)] has to be a non-negative integer, but is negative")


## any_src-can-deadlock10 
addProcessLocalUmpireTest ("umpire_any_src-can-deadlock10" "any_src-can-deadlock10.c" 3 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## any_src-can-deadlock11 
addProcessLocalUmpireTest ("umpire_any_src-can-deadlock11" "any_src-can-deadlock11.c" 3 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")

## any_src-can-deadlock2 
addProcessLocalUmpireTest ("umpire_any_src-can-deadlock2" "any_src-can-deadlock2.c" 3   
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")

## any_src-can-deadlock3
### Only tools such as ISP will see this deadlock, we do not run the test for that reason 
#addProcessLocalUmpireTest ("umpire_any_src-can-deadlock3" "any_src-can-deadlock3.c" 3
#  "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
#   "")
 
## any_src-can-deadlock4
### Deadlock only happens some times (random), we have a hard time to make this to a reliable check 
#addProcessLocalUmpireTest ("umpire_any_src-can-deadlock4" "any_src-can-deadlock4.c" 3 
#  "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
# "")

## any_src-can-deadlock5 
addProcessLocalUmpireTest ("umpire_any_src-can-deadlock5" "any_src-can-deadlock5.c" 3 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")

## any_src-can-deadlock6 
addProcessLocalUmpireTest ("umpire_any_src-can-deadlock6" "any_src-can-deadlock6.c" 3 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## any_src-can-deadlock7 
### Deadlock only happens some times (random), we have a hard time to make this to a reliable check
#addProcessLocalUmpireTest ("umpire_any_src-can-deadlock7" "any_src-can-deadlock7.c" 3 
#   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
#   "")

## any_src-can-deadlock8
### Only tools such as ISP will see this deadlock, we do not run the test for that reason 
#addProcessLocalUmpireTest ("umpire_any_src-can-deadlock8" "any_src-can-deadlock8.c" 3 
#   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
#   "")

## any_src-can-deadlock9
### Only tools such as ISP will see this deadlock, we do not run the test for that reason 
#addProcessLocalUmpireTest ("umpire_any_src-can-deadlock9" "any_src-can-deadlock9.c" 3
#  "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
#   "")


## any_src-can-deadlock
### Deadlock only happens some times (random), we have a hard time to make this to a reliable check 
#addProcessLocalUmpireTest ("umpire_any_src-can-deadlock" "any_src-can-deadlock.c" 3 
#  "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
#   "")



## any_src-deadlock 
addProcessLocalUmpireTest ("umpire_any_src-deadlock" "any_src-deadlock.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## any_src-waitall-deadlock2 
addProcessLocalUmpireTest ("umpire_any_src-waitall-deadlock2" "any_src-waitall-deadlock2.c" 2 
   "The application issued a set of MPI calls that can cause a deadlock!"
   "")


## any_src-waitall-deadlock3 
addProcessLocalUmpireTest ("umpire_any_src-waitall-deadlock3" "any_src-waitall-deadlock3.c" 3 
   "The application issued a set of MPI calls that can cause a deadlock!"
   "")


## any_src-waitall-deadlock 
addProcessLocalUmpireTest ("umpire_any_src-waitall-deadlock" "any_src-waitall-deadlock.c" 3 
   "The application issued a set of MPI calls that can cause a deadlock!"
   "")

## any_src-waitany-deadlock2 
addProcessLocalUmpireTest ("umpire_any_src-waitany-deadlock2" "any_src-waitany-deadlock2.c" 3
   "The application issued a set of MPI calls that can cause a deadlock!"
   "")

 
## any_src-waitany-deadlock 
addProcessLocalUmpireTest ("umpire_any_src-waitany-deadlock" "any_src-waitany-deadlock.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")

## any_src-wait-deadlock2 
addProcessLocalUmpireTest ("umpire_any_src-wait-deadlock2" "any_src-wait-deadlock2.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## any_src-wait-deadlock 
addProcessLocalUmpireTest ("umpire_any_src-wait-deadlock" "any_src-wait-deadlock.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## basic-deadlock 
addProcessLocalUmpireTest ("umpire_basic-deadlock" "basic-deadlock.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## basic-deadlock-cart_create 
addProcessLocalUmpireTest ("umpire_basic-deadlock-cart_create" "basic-deadlock-cart_create.c" 4 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")

#TODO: Doesn'T work correctly deadlock is not found but: YES, MUST CURRENTLY DOES NOT HANDLE MPI_CART_SUB, NEEDS TO BE ADDED
#      Error: from: call MPI_Comm_size@rank 2: Argument 1 (comm) is an unknown communicator where a valid communicator was expected
## basic-deadlock-cart_sub 
addProcessLocalUmpireTest ("umpire_basic-deadlock-cart_sub" "basic-deadlock-cart_sub.c" 4 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## basic-deadlock-comm_create 
addProcessLocalUmpireTest ("umpire_basic-deadlock-comm_create" "basic-deadlock-comm_create.c" 3 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## basic-deadlock-comm_dup 
addProcessLocalUmpireTest ("umpire_basic-deadlock-comm_dup" "basic-deadlock-comm_dup.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## basic-deadlock-comm_split 
addProcessLocalUmpireTest ("umpire_basic-deadlock-comm_split" "basic-deadlock-comm_split.c" 3 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## basic-deadlock-graph_create 
addProcessLocalUmpireTest ("umpire_basic-deadlock-graph_create" "basic-deadlock-graph_create.c" 4 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## basic-deadlock-intercomm_create 
addProcessLocalUmpireTest ("umpire_basic-deadlock-intercomm_create" "basic-deadlock-intercomm_create.c" 3 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")

#TODO: does not work correctly.. : YES, MUST CURRENTLY DOES NOT HANDLE INTERCOMMS, NEEDS TO BE ADDED
## basic-deadlock-intercomm_merge 
addProcessLocalUmpireTest ("umpire_basic-deadlock-intercomm_merge" "basic-deadlock-intercomm_merge.c" 3 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")

## bcast-deadlock 
addProcessLocalUmpireTest ("umpire_bcast-deadlock" "bcast-deadlock.c" 2 
   "Two collective operations that use a root process specified conflicting roots"
   "")

#TODO: This class of checks is currently not supported by MUST (checksums on contents of memory buffers)
## change-send-buffer 
#addProcessLocalUmpireTest ("umpire_change-send-buffer" "change-send-buffer.c" 2 
#  "ERROR;WARNING"
#   "")

#TODO: This class of checks is currently not supported by MUST (checksums on contents of memory buffers)
## change-send-buffer-exhaustive 
#addProcessLocalUmpireTest ("umpire_change-send-buffer-exhaustive" "change-send-buffer-exhaustive.c" 2 
#   "ERROR:"
#   "")

#TODO: This class of checks is currently not supported by MUST (checksums on contents of memory buffers)
## change-send-buffer-type-exhaustive 
#addProcessLocalUmpireTest ("umpire_change-send-buffer-type-exhaustive" "change-send-buffer-type-exhaustive.c" 2 
#   "ERROR;WARNING"
#   "")


## collective-exhaustive-byte-int-mismatch 
addProcessLocalUmpireTest ("umpire_collective-exhaustive-byte-int-mismatch" "collective-exhaustive-byte-int-mismatch.c" 2 
   "Error: from: call MPI_Bcast@.*[0-9]: Two collective calls cause a type mismatch!"
   "")

#TODO: Under investigation Integerger checks look at sendcounts of non-root tasks in scatterv, which should not happen
## collective-exhaustive-no-error 
addProcessLocalUmpireTest ("umpire_collective-exhaustive-no-error" "collective-exhaustive-no-error.c" 2 
   ""
   "Error;ERROR")


## collective-misorder2 
addProcessLocalUmpireTest ("umpire_collective-misorder2" "collective-misorder2.c" 2 
   "A collective mismatch occured"
   "")


## collective-misorder-allreduce 
addProcessLocalUmpireTest ("umpire_collective-misorder-allreduce" "collective-misorder-allreduce.c" 2 
   "A collective mismatch occured"
   "")

## collective-misorder 
addProcessLocalUmpireTest ("umpire_collective-misorder" "collective-misorder.c" 2 
   "A collective mismatch occured"
   "")


## comm-bcast-deadlock 
addProcessLocalUmpireTest ("umpire_comm-bcast-deadlock" "comm-bcast-deadlock.c" 3 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## comm-deadlock 
addProcessLocalUmpireTest ("umpire_comm-deadlock" "comm-deadlock.c" 2
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## comm-dup-no-error 
addProcessLocalUmpireTest ("umpire_comm-dup-no-error" "comm-dup-no-error.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## comm-dup-no-free2 
addProcessLocalUmpireTest ("umpire_comm-dup-no-free2" "comm-dup-no-free2.c" 2 
   "There are 1000 communicators that are not freed when MPI_Finalize was issued"
   "")


## comm-dup-no-free 
addProcessLocalUmpireTest ("umpire_comm-dup-no-free" "comm-dup-no-free.c" 2 
   "There are 1 communicators that are not freed when MPI_Finalize was issued"
   "")

## comm-simple 
addProcessLocalUmpireTest ("umpire_comm-simple" "comm-simple.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## comm-split-no-free 
addProcessLocalUmpireTest ("umpire_comm-split-no-free" "comm-split-no-free.c" 2 
   "There are 1 communicators that are not freed when MPI_Finalize was issued"
   "")

## comm-translate-ranks 
addProcessLocalUmpireTest ("umpire_comm-translate-ranks" "comm-translate-ranks.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## complex-deadlock 
addProcessLocalUmpireTest ("umpire_complex-deadlock" "complex-deadlock.c" 3
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## deadlock-config 
addProcessLocalUmpireTest ("umpire_deadlock-config" "deadlock-config.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## dropped-lost-req 
addProcessLocalUmpireTest ("umpire_dropped-lost-req" "dropped-lost-req.c" 2 
   "Error: from: call MPI_Irecv@.*1: The application fails to match a point-to-point operation"
   "")


## dropped-req 
addProcessLocalUmpireTest ("umpire_dropped-req" "dropped-req.c" 2 
   "Error: from: call MPI_Isend@.*0: The application fails to match a point-to-point operation before it issues MPI_Finalize"
   "")


## errhandler-no-error 
addProcessLocalUmpireTest ("umpire_errhandler-no-error" "errhandler-no-error.c" 2    
   ""
   "Error;ERROR;Warning;WARNING")


## errhandler-no-free 
addProcessLocalUmpireTest ("umpire_errhandler-no-free" "errhandler-no-free.c" 2 
   "There are 5 error handlers that are not freed when MPI_Finalize was issued"
   "")


## finalize-deadlock 
addProcessLocalUmpireTest ("umpire_finalize-deadlock" "finalize-deadlock.c" 2    
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## group-no-error2 
addProcessLocalUmpireTest ("umpire_group-no-error2" "group-no-error2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## group-no-error3 
addProcessLocalUmpireTest ("umpire_group-no-error3" "group-no-error3.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## group-no-error 
addProcessLocalUmpireTest ("umpire_group-no-error" "group-no-error.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## group-no-error-exhaustive 
addProcessLocalUmpireTest ("umpire_group-no-error-exhaustive" "group-no-error-exhaustive.c" 3 
   ""
   "Error;ERROR;Warning;WARNING")


## group-no-free2 
addProcessLocalUmpireTest ("umpire_group-no-free2" "group-no-free2.c" 2 
   "There are 1 groups that are not freed when MPI_Finalize was issued"
   "")


## group-no-free3 
addProcessLocalUmpireTest ("umpire_group-no-free3" "group-no-free3.c" 2 
   "There are 10 groups that are not freed when MPI_Finalize was issued"
   "")


## group-no-free 
addProcessLocalUmpireTest ("umpire_group-no-free" "group-no-free.c" 2 
   "There are 1 groups that are not freed when MPI_Finalize was issued"
   "")


## group-no-free-exhaustive 
addProcessLocalUmpireTest ("umpire_group-no-free-exhaustive" "group-no-free-exhaustive.c" 3 
   "There are 9 groups that are not freed when MPI_Finalize was issued"
   "")


## hello 
addProcessLocalUmpireTest ("umpire_hello" "hello.c" 2    
   ""
   "Error;ERROR;Warning;WARNING")


#TODO: this test runs in to a timeout: YES, MUST CURRENTLY DOES NOT HANDLE INTERCOMMS, NEEDS TO BE ADDED 
## intercomm_create-deadlock2 
addProcessLocalUmpireTest ("umpire_intercomm_create-deadlock2" "intercomm_create-deadlock2.c" 3  
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")

#TODO: this test runs in to a timeout: YES, MUST CURRENTLY DOES NOT HANDLE INTERCOMMS, NEEDS TO BE ADDED
## intercomm_create-deadlock3 
addProcessLocalUmpireTest ("umpire_intercomm_create-deadlock3" "intercomm_create-deadlock3.c" 4   
   "The application issued a set of MPI calls that can cause a deadlock!"
   "")


#TODO: this test runs in to a timeout: YES, MUST CURRENTLY DOES NOT HANDLE INTERCOMMS, NEEDS TO BE ADDED
## intercomm_create-deadlock4 
addProcessLocalUmpireTest ("umpire_intercomm_create-deadlock4" "intercomm_create-deadlock4.c" 5 
   "The application issued a set of MPI calls that can cause a deadlock!"
   "")

#TODO: this test runs in to a timeout: YES, MUST CURRENTLY DOES NOT HANDLE INTERCOMMS, NEEDS TO BE ADDED
## intercomm_create-deadlock 
addProcessLocalUmpireTest ("umpire_intercomm_create-deadlock" "intercomm_create-deadlock.c" 3    
   "The application issued a set of MPI calls that can cause a deadlock!"
   "")


## intercomm_create-no-error 
addProcessLocalUmpireTest ("umpire_intercomm_create-no-error" "intercomm_create-no-error.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


#TODO: this test runs in to a timeout: YES, MUST CURRENTLY DOES NOT HANDLE INTERCOMMS, NEEDS TO BE ADDED
## intercomm_merge-deadlock 
addProcessLocalUmpireTest ("umpire_intercomm_merge-deadlock" "intercomm_merge-deadlock.c" 4 
   "The application issued a set of MPI calls that can cause a deadlock!"
   "")


## irecv-deadlock 
addProcessLocalUmpireTest ("umpire_irecv-deadlock" "irecv-deadlock.c" 2 
   "The application issued a set of MPI calls that can cause a deadlock!"
   "")


## irecv-isend-ok2 
addProcessLocalUmpireTest ("umpire_irecv-isend-ok2" "irecv-isend-ok2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## irecv-isend-ok 
addProcessLocalUmpireTest ("umpire_irecv-isend-ok" "irecv-isend-ok.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## lost-request2 
addProcessLocalUmpireTest ("umpire_lost-request2" "lost-request2.c" 2 
   "There are .* requests that are not freed when MPI_Finalize "
   "")

## lost-request3
### Note: has dual error, memory overlap is a side effect of the lost requests 
addProcessLocalUmpireTest ("umpire_lost-request3" "lost-request3.c" 2 
   "The memory regions to be transfered by this receive operation overlap.*There are 1000 requests that are not freed when MPI_Finalize was issued"
   "")


## lost-request 
addProcessLocalUmpireTest ("umpire_lost-request" "lost-request.c" 2 
   "There are .* requests that are not freed when MPI_Finalize "
   "")


## lost-request-waitall 
addProcessLocalUmpireTest ("umpire_lost-request-waitall" "lost-request-waitall.c" 2 
   "There are .* requests that are not freed when MPI_Finalize "
   "")

## match-test 
addProcessLocalUmpireTest ("umpire_match-test" "match-test.c" 3 
   "The application issued a set of MPI calls that can cause a deadlock.*MPI_Send"
   "")


## match-test-persistent 
addProcessLocalUmpireTest ("umpire_match-test-persistent" "match-test-persistent.c" 3 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error2 
addProcessLocalUmpireTest ("umpire_no-error2" "no-error2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error3-any_src 
addProcessLocalUmpireTest ("umpire_no-error3-any_src" "no-error3-any_src.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error3 
addProcessLocalUmpireTest ("umpire_no-error3" "no-error3.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error4-any_src 
addProcessLocalUmpireTest ("umpire_no-error4-any_src" "no-error4-any_src.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error4 
addProcessLocalUmpireTest ("umpire_no-error4" "no-error4.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-any_src 
addProcessLocalUmpireTest ("umpire_no-error-any_src" "no-error-any_src.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error 
addProcessLocalUmpireTest ("umpire_no-error" "no-error.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


#TODO: uses MPI_Cart_sub which we don't support ATM
## no-error-derived-comms 
addProcessLocalUmpireTest ("umpire_no-error-derived-comms" "no-error-derived-comms.c" 4 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-interleaved-isend 
addProcessLocalUmpireTest ("umpire_no-error-interleaved-isend" "no-error-interleaved-isend.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-persistent-all-completions 
addProcessLocalUmpireTest ("umpire_no-error-persistent-all-completions" "no-error-persistent-all-completions.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-persistent 
addProcessLocalUmpireTest ("umpire_no-error-persistent" "no-error-persistent.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-persistent-test 
addProcessLocalUmpireTest ("umpire_no-error-persistent-test" "no-error-persistent-test.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-persistent-testpartial 
addProcessLocalUmpireTest ("umpire_no-error-persistent-testpartial" "no-error-persistent-testpartial.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-persistent-waitpartial 
addProcessLocalUmpireTest ("umpire_no-error-persistent-waitpartial" "no-error-persistent-waitpartial.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-probe-any_src 
addProcessLocalUmpireTest ("umpire_no-error-probe-any_src" "no-error-probe-any_src.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-probe-any_tag 
addProcessLocalUmpireTest ("umpire_no-error-probe-any_tag" "no-error-probe-any_tag.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-probe 
addProcessLocalUmpireTest ("umpire_no-error-probe" "no-error-probe.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-testall 
addProcessLocalUmpireTest ("umpire_no-error-testall" "no-error-testall.c" 3 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-testany 
addProcessLocalUmpireTest ("umpire_no-error-testany" "no-error-testany.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-test 
addProcessLocalUmpireTest ("umpire_no-error-test" "no-error-test.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-vector-isend 
addProcessLocalUmpireTest ("umpire_no-error-vector-isend" "no-error-vector-isend.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-wait2 
addProcessLocalUmpireTest ("umpire_no-error-wait2" "no-error-wait2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-waitall-any_src2 
addProcessLocalUmpireTest ("umpire_no-error-waitall-any_src2" "no-error-waitall-any_src2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-waitall-any_src3 
addProcessLocalUmpireTest ("umpire_no-error-waitall-any_src3" "no-error-waitall-any_src3.c" 3 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-waitall-any_src 
addProcessLocalUmpireTest ("umpire_no-error-waitall-any_src" "no-error-waitall-any_src.c" 3 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-waitall 
addProcessLocalUmpireTest ("umpire_no-error-waitall" "no-error-waitall.c" 3 
   ""
   "Error;ERROR;Warning;WARNING")

## no-error-waitany2 
addProcessLocalUmpireTest ("umpire_no-error-waitany2" "no-error-waitany2.c" 3
   ""
   "Error;ERROR")


## no-error-waitany-any_src2 
addProcessLocalUmpireTest ("umpire_no-error-waitany-any_src2" "no-error-waitany-any_src2.c" 3 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-waitany-any_src3 
addProcessLocalUmpireTest ("umpire_no-error-waitany-any_src3" "no-error-waitany-any_src3.c" 3 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-waitany-any_src 
addProcessLocalUmpireTest ("umpire_no-error-waitany-any_src" "no-error-waitany-any_src.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-waitany 
addProcessLocalUmpireTest ("umpire_no-error-waitany" "no-error-waitany.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-wait-any_src2 
addProcessLocalUmpireTest ("umpire_no-error-wait-any_src2" "no-error-wait-any_src2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-wait-any_src3 
addProcessLocalUmpireTest ("umpire_no-error-wait-any_src3" "no-error-wait-any_src3.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-wait-any_src4 
addProcessLocalUmpireTest ("umpire_no-error-wait-any_src4" "no-error-wait-any_src4.c" 3
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-wait-any_src 
addProcessLocalUmpireTest ("umpire_no-error-wait-any_src" "no-error-wait-any_src.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## no-error-wait 
addProcessLocalUmpireTest ("umpire_no-error-wait" "no-error-wait.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## op-no-error 
addProcessLocalUmpireTest ("umpire_op-no-error" "op-no-error.c" 1 
   ""
   "Error;ERROR;Warning;WARNING")


## op-no-free 
addProcessLocalUmpireTest ("umpire_op-no-free" "op-no-free.c" 1 
   "There are .* operations that are not freed when MPI_Finalize was issued"
   "")


## partial-recv 
addProcessLocalUmpireTest ("umpire_partial-recv" "partial-recv.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## partial-recv-exhaustive 
addProcessLocalUmpireTest ("umpire_partial-recv-exhaustive" "partial-recv-exhaustive.c" 2 
   "Error: from: call MPI_Irecv@.*.: The memory regions to be transfered by this receive operation overlap with regions spanned by a pending non-blocking operation!"
   "")

## partial-recv-persistent2 
addProcessLocalUmpireTest ("umpire_partial-recv-persistent2" "partial-recv-persistent2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")

## partial-recv-persistent3 
addProcessLocalUmpireTest ("umpire_partial-recv-persistent3" "partial-recv-persistent3.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## partial-recv-persistent4 
addProcessLocalUmpireTest ("umpire_partial-recv-persistent4" "partial-recv-persistent4.c" 3 
   ""
   "Error;ERROR;Warning;WARNING")

## partial-recv-persistent 
addProcessLocalUmpireTest ("umpire_partial-recv-persistent" "partial-recv-persistent.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## persistent2 
addProcessLocalUmpireTest ("umpire_persistent2" "persistent2.c" 2 
   "Error: from: call MPI_Start@.*1: Argument 1 [(]request[)] is already an active request!"
   "")


## persistent3 
addProcessLocalUmpireTest ("umpire_persistent3" "persistent3.c" 2 
   "Error: from: call MPI_Waitall@.*0: Argument 2 [(]array_of_requests[)] has to be an array of predefined or user defined requests"
   "")


## persistent4 
addProcessLocalUmpireTest ("umpire_persistent4" "persistent4.c" 3 
   "Error: from: call MPI_Waitall@.*1: Argument 2 [(]array_of_requests[)] has to be an array of predefined or user defined requests,"
   "")


## persistent 
addProcessLocalUmpireTest ("umpire_persistent" "persistent.c" 2 
   "Error: from: call MPI_Waitall@.*.: Argument 2 [(]array_of_requests[)] has to be an array of predefined or user defined requests, the following entries are unknown requests:"
   "")

### This is a test with a random error, we can't make this a test
## probe-any_src-types-can-mismatch 
#addProcessLocalUmpireTest ("umpire_probe-any_src-types-can-mismatch" "probe-any_src-types-can-mismatch.c" 3 
#   ""
#  "Error;ERROR;Warning;WARNING")
### probe-any_tag-types-can-mismatch 
#addProcessLocalUmpireTest ("umpire_probe-any_tag-types-can-mismatch" "probe-any_tag-types-can-mismatch.c" 2 
#   ""
#   "Error;ERROR;Warning;WARNING")


#TODO: We do not catch this deadlock as MUST does not supports MPI_Probe yet
## probe-deadlock 
addProcessLocalUmpireTest ("umpire_probe-deadlock" "probe-deadlock.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## pt2pt-byte-int-mismatch 
addProcessLocalUmpireTest ("umpire_pt2pt-byte-int-mismatch" "pt2pt-byte-int-mismatch.c" 2 
   "Error: from: call MPI_Irsend@.*0: A send and a receive operation use datatypes that do not match!"
   "")


## remote_group-no-error 
addProcessLocalUmpireTest ("umpire_remote_group-no-error" "remote_group-no-error.c" 3 
   ""
   "Error;ERROR;Warning;WARNING")


## sendrecv-deadlock 
addProcessLocalUmpireTest ("umpire_sendrecv-deadlock" "sendrecv-deadlock.c" 3 
   "The application issued a set of MPI calls that can cause a deadlock!"
   "")


## send-recv-ok 
addProcessLocalUmpireTest ("umpire_send-recv-ok" "send-recv-ok.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## type-commit-twice 
addProcessLocalUmpireTest ("umpire_type-commit-twice" "type-commit-twice.c" 1 
   "Warning: from: call MPI_Type_commit@.*0: Argument 1 [(]datatype[)] is already commited, there is no need to commit it again!"
   "")


## type-no-error 
addProcessLocalUmpireTest ("umpire_type-no-error" "type-no-error.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## type-no-error-exhaustive 
addProcessLocalUmpireTest ("umpire_type-no-error-exhaustive" "type-no-error-exhaustive.c" 1 
   ""
   "Error;ERROR;Warning;WARNING")


## type-no-error-exhaustive-with-isends 
addProcessLocalUmpireTest ("umpire_type-no-error-exhaustive-with-isends" "type-no-error-exhaustive-with-isends.c" 2 
   ""
   "Error;ERROR;Warning;WARNING")


## type-no-free2 
addProcessLocalUmpireTest ("umpire_type-no-free2" "type-no-free2.c" 1 
   "There are .* datatypes that are not freed when MPI_Finalize was issued"
   "")


## type-no-free3 
addProcessLocalUmpireTest ("umpire_type-no-free3" "type-no-free3.c" 2 
   "There are .* datatypes that are not freed when MPI_Finalize was issued"
   "")


## type-no-free 
addProcessLocalUmpireTest ("umpire_type-no-free" "type-no-free.c" 1 
   "There are .* datatypes that are not freed when MPI_Finalize was issued"
   "")


## type-no-free-exhaustive 
addProcessLocalUmpireTest ("umpire_type-no-free-exhaustive" "type-no-free-exhaustive.c" 1 
   "There are .* datatypes that are not freed when MPI_Finalize was issued"
   "")


## waitall-deadlock 
addProcessLocalUmpireTest ("umpire_waitall-deadlock" "waitall-deadlock.c" 3 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output"
   "")


## waitany-deadlock 
addProcessLocalUmpireTest ("umpire_waitany-deadlock" "waitany-deadlock.c" 2    
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output"
   "")


## wait-deadlock 
addProcessLocalUmpireTest ("umpire_wait-deadlock" "wait-deadlock.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output"
   "")

