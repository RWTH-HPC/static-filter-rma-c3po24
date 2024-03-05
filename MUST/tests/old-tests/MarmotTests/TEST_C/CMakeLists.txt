# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for DatatypeTrack test.
#
# @author Mathias Korepkat
# @date 10.01.2012
#

##################
#Recurse into sub directories
##Not needed

###################


MACRO (addProcessLocalMarmotTest
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
ENDMACRO (addProcessLocalMarmotTest)

#Add test case


## abort 
addProcessLocalMarmotTest ("marmot_c_abort" "abort.c" 1 
   "Warning: from: call MPI_Abort@.*: Argument 1 [(]comm[)] is MPI_COMM_NULL, which is allowed but unusual"
   "")


## all2all 
addProcessLocalMarmotTest ("marmot_c_all2all" "all2all.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## all2allv 
addProcessLocalMarmotTest ("marmot_c_all2allv" "all2allv.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## allgather 
addProcessLocalMarmotTest ("marmot_c_allgather" "allgather.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## allgatherv 
addProcessLocalMarmotTest ("marmot_c_allgatherv" "allgatherv.c" 4 
   "Warning: from: call MPI_Allgatherv@.*: Argument 5 [(]recvcounts[)] is an array that contains zero value[(]s[)], which is correct but unusual"
   "")


## allreduce 
addProcessLocalMarmotTest ("marmot_c_allreduce" "allreduce.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## barrier 
addProcessLocalMarmotTest ("marmot_c_barrier" "barrier.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## basic 
addProcessLocalMarmotTest ("marmot_c_basic" "basic.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## basic_mult2 
addProcessLocalMarmotTest ("marmot_c_basic_mult2" "basic_mult2.c" 3 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## basic_mult 
addProcessLocalMarmotTest ("marmot_c_basic_mult" "basic_mult.c" 3 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## basic_pt2pt 
addProcessLocalMarmotTest ("marmot_c_basic_pt2pt" "basic_pt2pt.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## bcast 
addProcessLocalMarmotTest ("marmot_c_bcast" "bcast.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## bsend 
addProcessLocalMarmotTest ("marmot_c_bsend" "bsend.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## buffer_reuse 
addProcessLocalMarmotTest ("marmot_c_buffer_reuse" "buffer_reuse.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## cancel 
addProcessLocalMarmotTest ("marmot_c_cancel" "cancel.c" 2 
   "P2PMatch: detected a cancel, not supported, outputs may be wrong!"
   "")


## cart-coords 
addProcessLocalMarmotTest ("marmot_c_cart-coords" "cart-coords.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## cart-coords-nvcomm 
addProcessLocalMarmotTest ("marmot_c_cart-coords-nvcomm" "cart-coords-nvcomm.c" 2 
   "Error: from: call MPI_Cart_coords@.*: Argument 1 [(]comm[)] is a communicator with no cartesian topology and was used where such a topology is required"
   "")


## cart-create 
addProcessLocalMarmotTest ("marmot_c_cart-create" "cart-create.c" 6 
   "There are 1 communicators that are not freed when MPI_Finalize was issued"
   "")


## cart-rank 
addProcessLocalMarmotTest ("marmot_c_cart-rank" "cart-rank.c" 6 
   "There are 1 communicators that are not freed when MPI_Finalize was issued,"
   "")

## cart-rank-nvcomm 
addProcessLocalMarmotTest ("marmot_c_cart-rank-nvcomm" "cart-rank-nvcomm.c" 2 
   "Error: from: call MPI_Cart_rank@.*: Argument 1 [(]comm[)] is a communicator with no cartesian topology and was used where such a topology is required"
   "")


## cart-shift 
addProcessLocalMarmotTest ("marmot_c_cart-shift" "cart-shift.c" 6 
   "There are 1 communicators that are not freed when MPI_Finalize was issued, a quality application should free all MPI resources before calling MPI_Finalize"
   "")


## cart-shift-nvcomm 
addProcessLocalMarmotTest ("marmot_c_cart-shift-nvcomm" "cart-shift-nvcomm.c" 2 
   "Error: from: call MPI_Cart_shift@.*: Argument 1 [(]comm[)] is a communicator with no cartesian topology and was used where such a topology is required"
   "")


## cg-tutorial-marmot-exercise 
addProcessLocalMarmotTest ("marmot_c_cg-tutorial-marmot-exercise" "cg-tutorial-marmot-exercise.c" 2 
   "Warning: from: call MPI_Type_struct@.*: Argument 2 [(]array_of_blocklengths[)] is an array that contains zero value[(]s[)], which is correct but unusual"
   "")


## comm-compare 
addProcessLocalMarmotTest ("marmot_c_comm-compare" "comm-compare.c" 6
   "There are 1 communicators that are not freed when MPI_Finalize was issued"
   "")


## comm-create2 
addProcessLocalMarmotTest ("marmot_c_comm-create2" "comm-create2.c" 6 
   "Warning: from: call MPI_Comm_create@.*: Argument 1 [(]comm[)] is a communicator that already had a process topology."
   "")


## comm-create 
addProcessLocalMarmotTest ("marmot_c_comm-create" "comm-create.c" 6 
   "There are 1 groups that are not freed when MPI_Finalize was issued,"
   "")

## comm-create-f2 
addProcessLocalMarmotTest ("marmot_c_comm-create-f2" "comm-create-f2.c" 2 
   "There are . groups that are not freed when MPI_Finalize was issued,"
   "")


## comm-create-f 
addProcessLocalMarmotTest ("marmot_c_comm-create-f" "comm-create-f.c" 2 
   "Error: from: call MPI_Comm_create@.*: Argument 2 [(]group[)] is a group which should be a subset of argument 1[(]comm[)], but the following ranks are in the group but not in the communicator"
   "")


## comm_dup2 
addProcessLocalMarmotTest ("marmot_c_comm_dup2" "comm_dup2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## comm_dup 
addProcessLocalMarmotTest ("marmot_c_comm_dup" "comm_dup.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## deadlock1 
addProcessLocalMarmotTest ("marmot_c_deadlock1" "deadlock1.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")

#Todo: There is no Deadlock, because of the use of Isend and Irecv in the correct order with the correct tags
## deadlock2 
addProcessLocalMarmotTest ("marmot_c_deadlock2" "deadlock2.c" 2 
   "Error: from: call MPI_Isend@.*: The memory regions to be transfered by this send operation overlap with regions spanned by a pending non-blocking receive operation!"
   "")

## dims-create 
addProcessLocalMarmotTest ("marmot_c_dims-create" "dims-create.c" 2 
   "There are 1 communicators that are not freed when MPI_Finalize was issued"
   "")


## gather 
addProcessLocalMarmotTest ("marmot_c_gather" "gather.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## gatherv 
addProcessLocalMarmotTest ("marmot_c_gatherv" "gatherv.c" 4 
   "Warning: from: call MPI_Gatherv@.*: Argument 5 [(]recvcounts[)] is an array that contains zero value[(]s[)]"
   "")


## get-count 
addProcessLocalMarmotTest ("marmot_c_get-count" "get-count.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## get-version2 
addProcessLocalMarmotTest ("marmot_c_get-version2" "get-version2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## get-version 
addProcessLocalMarmotTest ("marmot_c_get-version" "get-version.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## graph-create 
addProcessLocalMarmotTest ("marmot_c_graph-create" "graph-create.c" 4 
   "There are 1 communicators that are not freed when MPI_Finalize was issued"
   "")


## graphs 
addProcessLocalMarmotTest ("marmot_c_graphs" "graphs.c" 4 
   "There are 1 communicators that are not freed when MPI_Finalize was issued"
   "")


## group-compare 
addProcessLocalMarmotTest ("marmot_c_group-compare" "group-compare.c" 2 
   "There are 3 groups that are not freed when MPI_Finalize was issued, a quality application should free all MPI resources before calling MPI_Finalize"
   "")


## group-exclude 
addProcessLocalMarmotTest ("marmot_c_group-exclude" "group-exclude.c" 2 
   "There are 2 groups that are not freed when MPI_Finalize was issued, a quality application should free all MPI resources"
   "")

## group-exclude-f 
addProcessLocalMarmotTest ("marmot_c_group-exclude-f" "group-exclude-f.c" 2 
   "Error: from: call MPI_Group_excl@.*: Argument . [(]ranks[)] is an array of ranks that must be in the given MPI group, the following entries do not match this criteria: ranks.*!"
   "")


## group-free 
addProcessLocalMarmotTest ("marmot_c_group-free" "group-free.c" 2 
   "Error: from: call MPI_Group_free@.*: Argument 1 [(]group[)] is an unknown group where a valid group was expected"
   "")


## group-include 
addProcessLocalMarmotTest ("marmot_c_group-include" "group-include.c" 2 
   "There are 1 groups that are not freed when MPI_Finalize was issued, a quality application should free all MPI resources before calling MPI_Finalize"
   "")

#this test needs at least 4 ranks
## group-include-f2 
addProcessLocalMarmotTest ("marmot_c_group-include-f2" "group-include-f2.c" 4 
   "Error: from: call MPI_Group_incl@.*: Argument .* [(]ranks[)] is an array of ranks where no duplications are allowed, the fellowing entries are duplicated:"
   "")



## group-include-f3 
addProcessLocalMarmotTest ("marmot_c_group-include-f3" "group-include-f3.c" 2 
   "Error: from: call MPI_Group_incl@.*: Argument 2 [(]n[)] is greater then the size of the MPI group,"
   "")

#TODO: A none- existing rank is added to a new group .. this is not found by must.
## group-include-f 
addProcessLocalMarmotTest ("marmot_c_group-include-f" "group-include-f.c" 2 
   ""
   "")


## group-ranges 
addProcessLocalMarmotTest ("marmot_c_group-ranges" "group-ranges.c" 2 
   "Error: from: call MPI_Group_range_incl@.*: Argument 3 [(]ranges[)] is an array of triplets of the form"
   "")


## group-translate-ranks2 
addProcessLocalMarmotTest ("marmot_c_group-translate-ranks2" "group-translate-ranks2.c" 6 
   "Error: from: call MPI_Comm_group@.*: Argument 1 [(]comm[)] is MPI_COMM_NULL where a valid communicator was expected"
   "")


## group-translate-ranks 
addProcessLocalMarmotTest ("marmot_c_group-translate-ranks" "group-translate-ranks.c" 6 
   "There are 3 groups that are not freed when MPI_Finalize was issued"
   "")


## ibsend-rr 
addProcessLocalMarmotTest ("marmot_c_ibsend-rr" "ibsend-rr.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## initialized 
addProcessLocalMarmotTest ("marmot_c_initialized" "initialized.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## iprobe 
addProcessLocalMarmotTest ("marmot_c_iprobe" "iprobe.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## irreproduce1 
addProcessLocalMarmotTest ("marmot_c_irreproduce1" "irreproduce1.c" 3 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## isc2008-example 
addProcessLocalMarmotTest ("marmot_c_isc2008-example" "isc2008-example.c" 2 
   "Error: from: call MPI_Cart_shift@.*: Argument 1 [(]comm[)] is MPI_COMM_NULL where a valid communicator was expected"
   "")
target_link_libraries(marmot_c_isc2008-example "m")


## isend 
addProcessLocalMarmotTest ("marmot_c_isend" "isend.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## issend-rr 
addProcessLocalMarmotTest ("marmot_c_issend-rr" "issend-rr.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")

#TODO mpi2 error, that is not found by must
## Currently disabled due to lack of feature tests for these MPI2 calls
## mpi2_get2 
#addProcessLocalMarmotTest ("marmot_c_mpi2_get2" "mpi2_get2.c" 2 
#   ""
#   "Error;ERROR;Warning;WARNING;Program received signal")

IF (HAVE_MPI_WCHAR AND HAVE_MPI_SIGNED_CHAR)
    ## mpi2_type_struct_optional 
    addProcessLocalMarmotTest ("marmot_c_mpi2_type_struct_optional" "mpi2_type_struct_optional.c" 2 
       "Warning: from: call MPI_Send@.*: Argument 2 [(]count[)] is zero, which is correct but unusual!"
       "")
ENDIF (HAVE_MPI_WCHAR AND HAVE_MPI_SIGNED_CHAR)


## mpi2_type_struct_optional_err_f 
## We do not use this test, it can cause compilation errors
#addProcessLocalMarmotTest ("marmot_c_mpi2_type_struct_optional_err_f" "mpi2_type_struct_optional_err_f.c" 2 
#   ""
#   "Error;ERROR;Warning;WARNING;Program received signal")

#TODO: no omp integration 
## omp_coll_comm 
#addProcessLocalMarmotTest ("marmot_c_omp_coll_comm" "omp_coll_comm.c" 4 
#   ""
#   "Error;ERROR;Warning;WARNING;Program received signal")


## pack2 
addProcessLocalMarmotTest ("marmot_c_pack2" "pack2.c" 3 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## pack3 
addProcessLocalMarmotTest ("marmot_c_pack3" "pack3.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## pack4 
addProcessLocalMarmotTest ("marmot_c_pack4" "pack4.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## pending-msg 
addProcessLocalMarmotTest ("marmot_c_pending-msg" "pending-msg.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")


## pers_requests1 
addProcessLocalMarmotTest ("marmot_c_pers_requests1" "pers_requests1.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## pers_requests2 
addProcessLocalMarmotTest ("marmot_c_pers_requests2" "pers_requests2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## pers_requests3 
addProcessLocalMarmotTest ("marmot_c_pers_requests3" "pers_requests3.c" 2 
   "There are 2 requests that are not freed when MPI_Finalize was issued,"
   "")



## pers_requests_nor 
addProcessLocalMarmotTest ("marmot_c_pers_requests_nor" "pers_requests_nor.c" 2 
   "Warning: from: call MPI_Startall@.*: Argument 1 [(]count[)] is zero, which is correct but unusual!"
   "")

## pers_requests_npr 
addProcessLocalMarmotTest ("marmot_c_pers_requests_npr" "pers_requests_npr.c" 2 
   "Error: from: call MPI_Start@.*: Argument 1 [(]request[)] is already an active request!"
   "")


## probe 
addProcessLocalMarmotTest ("marmot_c_probe" "probe.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## proc-null 
addProcessLocalMarmotTest ("marmot_c_proc-null" "proc-null.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## reduce 
addProcessLocalMarmotTest ("marmot_c_reduce" "reduce.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")

##  warning about rank4 getting 0 values
## reduce_scatter 
addProcessLocalMarmotTest ("marmot_c_reduce_scatter" "reduce_scatter.c" 5 
   "Warning: from: call MPI_Reduce_scatter@.*: Argument . [(]recvcounts[)] is an array that contains zero value[(]s[)], which is correct but unusual. [(]recvcounts.*=0[)]"
   "")


## request_equiv 
addProcessLocalMarmotTest ("marmot_c_request_equiv" "request_equiv.c" 2 
   "Error: from: call MPI_Irecv@.*: The memory regions to be transfered by this receive operation overlap with regions spanned by a pending non-blocking operation!"
   "")


## request-non-valid 
addProcessLocalMarmotTest ("marmot_c_request-non-valid" "request-non-valid.c" 2 
   "Error: from: call MPI_Wait@.*: Argument 1 [(]request[)] is a unknown request"
   "")


#TODO: A Request gets lost.. This is not documented by MUST, because of the deadlock
## request-reuse1 
addProcessLocalMarmotTest ("marmot_c_request-reuse1" "request-reuse1.c" 2 
   "ERROR: MUST detected a deadlock, detailed information is available in the MUST output file"
   "")

## rsend 
addProcessLocalMarmotTest ("marmot_c_rsend" "rsend.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## scan 
addProcessLocalMarmotTest ("marmot_c_scan" "scan.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## scatter 
addProcessLocalMarmotTest ("marmot_c_scatter" "scatter.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## scatterv 
addProcessLocalMarmotTest ("marmot_c_scatterv" "scatterv.c" 4 
   "Warning: from: call MPI_Scatterv@.*: Argument 6 [(]recvcount[)] is zero, which is correct but unusual"
   "")


## sendrecv 
addProcessLocalMarmotTest ("marmot_c_sendrecv" "sendrecv.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## sendrecv_replace 
addProcessLocalMarmotTest ("marmot_c_sendrecv_replace" "sendrecv_replace.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## ssend 
addProcessLocalMarmotTest ("marmot_c_ssend" "ssend.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")

#TODO: no omp integration 
## sync_test 
#addProcessLocalMarmotTest ("marmot_c_sync_test" "sync_test.c" 4
#   ""
#   "Error;ERROR;Warning;WARNING;Program received signal")


## test_1 
addProcessLocalMarmotTest ("marmot_c_test_1" "test_1.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## test_2 
addProcessLocalMarmotTest ("marmot_c_test_2" "test_2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")

#TODO: the unknown request is not found 
## testany-noreq 
addProcessLocalMarmotTest ("marmot_c_testany-noreq" "testany-noreq.c" 2 
   "Warning: from: call MPI_Testany@.*: Argument . [(]count[)] is zero, which is correct but unusual!"
   "")


## testany-nvr2 
addProcessLocalMarmotTest ("marmot_c_testany-nvr2" "testany-nvr2.c" 2 
   "Error: from: call MPI_Testany@.*: Argument 2 [(]array_of_requests[)] has to be an array of predefined or user defined requests, the following entries are unknown requests"
   "")


## testany-nvr 
addProcessLocalMarmotTest ("marmot_c_testany-nvr" "testany-nvr.c" 2 
   "Error: from: call MPI_Testany@.*: Argument 2 [(]array_of_requests[)] has to be an array of predefined or user defined requests,"
   "")


## test_Ipt2pt 
addProcessLocalMarmotTest ("marmot_c_test_Ipt2pt" "test_Ipt2pt.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## twc2 
addProcessLocalMarmotTest ("marmot_c_twc2" "twc2.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## twc 
addProcessLocalMarmotTest ("marmot_c_twc" "twc.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## type_con 
addProcessLocalMarmotTest ("marmot_c_type_con" "type_con.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## type_size 
addProcessLocalMarmotTest ("marmot_c_type_size" "type_size.c" 2 
   "There are 2 datatypes that are not freed when MPI_Finalize was issued"
   "")


## typestruct-64bit 
addProcessLocalMarmotTest ("marmot_c_typestruct-64bit" "typestruct-64bit.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## type_struct_valid1 
addProcessLocalMarmotTest ("marmot_c_type_struct_valid1" "type_struct_valid1.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## waitall-nvr 
addProcessLocalMarmotTest ("marmot_c_waitall-nvr" "waitall-nvr.c" 2 
   "Error: from: call MPI_Waitall@.*: Argument 2 [(]array_of_requests[)] has to be an array of predefined or user defined requests,"
   "")


## waitany1 
addProcessLocalMarmotTest ("marmot_c_waitany1" "waitany1.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")


## waitany-noreq 
addProcessLocalMarmotTest ("marmot_c_waitany-noreq" "waitany-noreq.c" 2 
   "Warning: from: call MPI_Waitany@.*: Argument 1 [(]count[)] is zero, which is correct but unusual!"
   "")


## waitany-nvr 
addProcessLocalMarmotTest ("marmot_c_waitany-nvr" "waitany-nvr.c" 2 
   "Error: from: call MPI_Waitany@.*: Argument 2 [(]array_of_requests[)] has to be an array of predefined or user defined requests, the following entries are unknown requests"
   "")


## wait 
addProcessLocalMarmotTest ("marmot_c_wait" "wait.c" 2 
   "Warning: from: call MPI_Wait@.*: Argument 1 [(]request[)] is MPI_REQUEST_NULL was this intended?"
   "")


## waitsome1 
addProcessLocalMarmotTest ("marmot_c_waitsome1" "waitsome1.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")

## waitsome 
addProcessLocalMarmotTest ("marmot_c_waitsome" "waitsome.c" 2 
   ""
   "Error;ERROR;Warning;WARNING;Program received signal")

## waitsome-nvr 
addProcessLocalMarmotTest ("marmot_c_waitsome-nvr" "waitsome-nvr.c" 2 
   "Error: from: call MPI_Waitsome@.*: Argument 2 [(]array_of_requests[)] has to be an array of predefined or user defined requests, the following entries are unknown requests"
   "")
