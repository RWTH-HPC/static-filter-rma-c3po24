# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

include(CheckFortranMPISymbolExists)
include(CheckMPIFunctionExists)
include(CheckMPISymbolExists)
include(CheckMPISymbolIsRvalue)
include(CheckMPITypeExists)


#
# helper scripts
#

# Call check_mpi_function_exists and set _PREFIX and _POSTFIX variables.
macro (check_mpi_function_exists_pp function)
    STRING(TOUPPER "HAVE_${function}" havevariable)

    # check for symbol
#    check_mpi_function_exists(${function} ${havevariable})
    check_mpi_symbol_is_rvalue(${function} ${havevariable})

    # Set empty prefix and postfix if symbol was found or set an XML prefix if
    # symbol was not found.
    if (${havevariable})
        set(${havevariable}_PREFIX "")
        set(${havevariable}_POSTFIX "")
    else (${havevariable})
        set(${havevariable}_PREFIX "<!--")
        set(${havevariable}_POSTFIX "-->")
    endif(${havevariable})
endmacro (check_mpi_function_exists_pp)

macro (add_mpi_function_exists_pp function_list)
    STRING(TOUPPER "HAVE_${func}" havevariable)
    set(${havevariable}_PREFIX "")
    set(${havevariable}_POSTFIX "")
    set(${havevariable} true)
endmacro (add_mpi_function_exists_pp)

# CheckSymbolExists does not consider typedefs or enums
macro (check_mpi_type_exists_pp type)
    STRING(TOUPPER "HAVE_${type}" havevariable)
    check_mpi_type_exists(${type} ${havevariable})
endmacro (check_mpi_type_exists_pp)

# call check_mpi_symbol_exists and check for <function>_c2f and <function>_f2c
macro (check_mpi_function_exists_c2f2c function)
    STRING(TOUPPER "HAVE_${function}" variable)

    # check for symbols
    check_mpi_function_exists(${function}_f2c "${variable}_F2C")
    if (${${variable}_F2C})
        check_mpi_function_exists(${function}_c2f "${variable}_C2F")
    endif ()
endmacro (check_mpi_function_exists_c2f2c)

SET (NewMpi40Symbols
    MPI_Session
    MPI_Count
)

 foreach (symbol ${NewMpi40Symbols})
     check_mpi_type_exists_pp(${symbol})
 endforeach()

# optional Fortran datatypes
if (GTI_ENABLE_FORTRAN)
    set(OptionalFortranTypes
            MPI_DOUBLE_COMPLEX
            MPI_REAL2 MPI_REAL4 MPI_REAL8 MPI_REAL16
            MPI_INTEGER1 MPI_INTEGER2 MPI_INTEGER4 MPI_INTEGER8 MPI_INTEGER16
            MPI_COMPLEX8 MPI_COMPLEX16 MPI_COMPLEX32
            MPI_LOGICAL1 MPI_LOGICAL2 MPI_LOGICAL4 MPI_LOGICAL8 MPI_LOGICAL16
            MPI_2COMPLEX
            MPI_2DOUBLE_COMPLEX
        CACHE INTERNAL "All the optional Fortran types"
    )

    foreach (type ${OptionalFortranTypes})
        check_fortran_mpi_symbol_exists(${type} HAVE_${type})

        # Set extra variable used in Fortran source for datatype handling
        if (HAVE_${type})
            set(F_${type} "${type}" CACHE INTERNAL "")
        else (HAVE_${type})
            set(F_${type} "MPI_DATATYPE_NULL" CACHE INTERNAL "")
        endif (HAVE_${type})
    endforeach (type)
endif (GTI_ENABLE_FORTRAN)

# New MPI 3 datatypes
# We use the _MPI_DATATYPE-suffix here to distinguish from the C dataypes, e.g. MPI_Aint
check_mpi_symbol_is_rvalue(MPI_AINT HAVE_MPI_AINT_MPI_DATATYPE)
check_mpi_symbol_is_rvalue(MPI_OFFSET HAVE_MPI_OFFSET_MPI_DATATYPE)
check_mpi_symbol_is_rvalue(MPI_COUNT HAVE_MPI_COUNT_MPI_DATATYPE)

# optional datatypes C
check_mpi_symbol_is_rvalue(MPI_LONG_LONG_INT HAVE_MPI_LONG_LONG_INT)
check_mpi_symbol_is_rvalue(MPI_LONG_LONG HAVE_MPI_LONG_LONG)
check_mpi_symbol_is_rvalue(MPI_UNSIGNED_LONG_LONG HAVE_MPI_UNSIGNED_LONG_LONG)
check_mpi_symbol_is_rvalue(MPI_WCHAR HAVE_MPI_WCHAR)
check_mpi_symbol_is_rvalue(MPI_SIGNED_CHAR HAVE_MPI_SIGNED_CHAR)

# optional datatypes C++
check_mpi_symbol_is_rvalue(MPI_CXX_BOOL HAVE_MPI_CXX_BOOL)
check_mpi_symbol_is_rvalue(MPI_CXX_FLOAT_COMPLEX HAVE_MPI_CXX_FLOAT_COMPLEX)
check_mpi_symbol_is_rvalue(MPI_CXX_DOUBLE_COMPLEX HAVE_MPI_CXX_DOUBLE_COMPLEX)
check_mpi_symbol_is_rvalue(MPI_CXX_LONG_DOUBLE_COMPLEX HAVE_MPI_CXX_LONG_DOUBLE_COMPLEX)

# removed names and functions
set(Removed30Symbols 
                     MPI_LB 
                     MPI_UB 
                     MPI_Handler_function 
                     MPI_COMBINER_HVECTOR_INTEGER 
                     MPI_COMBINER_HINDEXED_INTEGER 
                     MPI_COMBINER_STRUCT_INTEGER)

foreach (symbol ${Removed30Symbols})
    check_mpi_symbol_is_rvalue(${symbol} HAVE_${symbol})
endforeach()

set(Removed30Functions 
    MPI_Keyval_free
    MPI_Keyval_create
    MPI_Attr_put
    MPI_Attr_get
    MPI_Attr_delete
    MPI_Type_ub
    MPI_Type_struct
    MPI_Type_lb
    MPI_Type_hvector
    MPI_Type_hindexed
    MPI_Type_extent
    MPI_Errhandler_get
    MPI_Errhandler_set
    MPI_Errhandler_create
    MPI_Address
)

foreach (function ${Removed30Functions})
    check_mpi_function_exists_pp(${function} HAVE_${function})
endforeach()

# get basic types of MPI datatypes
set(COMM_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_Comm_c2f.")
set(DATATYPE_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_Datatype_c2f.")
set(INFO_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_Info_c2f.")
set(ERRHANDLER_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_Errhandler_c2f.")
set(GROUP_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_Group_c2f.")
set(OP_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_Op_c2f.")
set(REQUEST_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_Request_c2f.")
set(WIN_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_Win_c2f.")
set(FILE_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_File_c2f.")
set(KEYVAL_TYPE "int" CACHE INTERNAL "Type to use for keyvals.")
set(AINT_TYPE "int64_t" CACHE INTERNAL "Type that is equal to MPI_Aint.")
set(COUNT_TYPE "int64_t" CACHE INTERNAL "Type that is equal to MPI_Count.")
set(SESSION_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_Session_c2f.")
set(MESSAGE_F_TYPE "uint64_t" CACHE INTERNAL "Return type of MPI_Message_c2f.")


# handle convert macros
set(convertMacroTypes
    MPI_Type
)
if (${ALL_FEATURE_TESTS})
    list(APPEND convertMacroTypes
        MPI_Comm
        MPI_Errhandler
        MPI_File
        MPI_Group
        MPI_Info
        MPI_Message
        MPI_Op
        MPI_Request
        MPI_Status
        MPI_Win
    )
endif (${ALL_FEATURE_TESTS})
foreach (type ${convertMacroTypes})
    check_mpi_function_exists_c2f2c(${type}) 
endforeach ()


#
# MPI 2
#

# MPI-2 constants
check_mpi_symbol_is_rvalue(MPI_DISTRIBUTE_BLOCK HAVE_MPI_DISTRIBUTE_BLOCK)
check_mpi_symbol_is_rvalue(MPI_ORDER_C HAVE_MPI_ORDER_C)
check_mpi_symbol_is_rvalue(MPI_STATUS_IGNORE HAVE_MPI_STATUS_IGNORE)
check_mpi_symbol_is_rvalue(MPI_STATUSES_IGNORE HAVE_MPI_STATUSES_IGNORE)

SET(new_mpi_20_functions 
    MPI_Alltoallw MPI_Exscan MPI_Get_address
    MPI_Reduce_scatter MPI_Reduce_scatter_block
    MPI_Type_create_darray MPI_Type_create_hindexed
    MPI_Type_create_hvector MPI_Type_create_indexed_block
    MPI_Type_create_resized MPI_Type_create_struct
    MPI_Type_create_subarray MPI_Type_get_extent
    MPI_Type_get_true_extent MPI_Comm_create_keyval
)

IF (NOT ${ALL_FEATURE_TESTS} AND ${MPI_C_VERSION} VERSION_GREATER_EQUAL "2")
    foreach (func ${new_mpi_20_functions})
        add_mpi_function_exists_pp(${func})
    endforeach (func)
ELSE()
    FOREACH (new_func ${new_mpi_20_functions})
        check_mpi_function_exists_pp(${new_func})
    ENDFOREACH ()
ENDIF()

# MPI 2.2 constants
foreach (type
    MPI_C_BOOL
    MPI_INT8_T MPI_INT16_T MPI_INT32_T MPI_INT64_T
    MPI_UINT8_T MPI_UINT16_T MPI_UINT32_T MPI_UINT64_T
    MPI_C_COMPLEX MPI_C_FLOAT_COMPLEX MPI_C_DOUBLE_COMPLEX
        MPI_C_LONG_DOUBLE_COMPLEX
)
    check_mpi_symbol_is_rvalue(${type} HAVE_${type})
endforeach (type)


#
# MPI 3
#

check_mpi_symbol_is_rvalue(MPI_DIST_GRAPH HAVE_MPI_DIST_GRAPH)


SET(new_mpi_30_functions 
    MPI_Dist_graph_neighbors_count
    MPI_Iallgather
    MPI_Iallgatherv
    MPI_Iallreduce
    MPI_Ialltoall
    MPI_Ialltoallv
    MPI_Ialltoallw
    MPI_Ibarrier
    MPI_Ibcast
    MPI_Iexscan
    MPI_Igather
    MPI_Igatherv
    MPI_Ineighbor_allgather
    MPI_Ineighbor_allgatherv
    MPI_Ineighbor_alltoall
    MPI_Ineighbor_alltoallv
    MPI_Ineighbor_alltoallw
    MPI_Ireduce
    MPI_Ireduce_scatter
    MPI_Ireduce_scatter_block
    MPI_Iscan
    MPI_Iscatter
    MPI_Iscatterv
    MPI_Neighbor_allgather
    MPI_Neighbor_allgatherv
    MPI_Neighbor_alltoall
    MPI_Neighbor_alltoallv
    MPI_Neighbor_alltoallw
    MPI_Reduce_local
    MPI_Comm_get_errhandler
    MPI_Comm_set_errhandler
)

IF (NOT ${ALL_FEATURE_TESTS} AND ${MPI_C_VERSION} VERSION_GREATER_EQUAL "3")
    foreach (func ${new_mpi_30_functions})
        add_mpi_function_exists_pp(${func})
    endforeach (func)
ELSE()
    FOREACH (new_func ${new_mpi_30_functions})
        check_mpi_function_exists_pp(${new_func})
    ENDFOREACH ()
ENDIF()

check_mpi_function_exists_pp(MPI_Type_dup)
check_mpi_function_exists_pp(MPI_Type_get_envelope)
check_mpi_function_exists_pp(MPI_Type_get_contents)
#
# MPI-3 RMA
#
SET(mpi3_rma_functions
    MPI_Win_create
    MPI_Win_allocate
    MPI_Win_allocate_shared
    MPI_Win_attach
    MPI_Win_c2f
    MPI_Win_call_errhandler
    MPI_Win_complete
    MPI_Win_create
    MPI_Win_create_dynamic
    MPI_Win_create_errhandler
    MPI_Win_create_keyval
    MPI_Win_delete_attr
    MPI_Win_detach
    MPI_Win_f2c
    MPI_Win_fence
    MPI_Win_flush
    MPI_Win_flush_all
    MPI_Win_flush_local
    MPI_Win_flush_local_all
    MPI_Win_free
    MPI_Win_free_keyval
    MPI_Win_get_attr
    MPI_Win_get_errhandler
    MPI_Win_get_group
    MPI_Win_get_info
    MPI_Win_get_name
    MPI_Win_lock
    MPI_Win_lock_all
    MPI_Win_post
    MPI_Win_set_attr
    MPI_Win_set_errhandler
    MPI_Win_set_info
    MPI_Win_set_name
    MPI_Win_shared_query
    MPI_Win_start
    MPI_Win_sync
    MPI_Win_test
    MPI_Win_unlock
    MPI_Win_unlock_all
    MPI_Win_wait
    MPI_Get
    MPI_Get_accumulate
    MPI_Rget
    MPI_Rget_accumulate
    MPI_Put
    MPI_Rput
    MPI_Accumulate
    MPI_Raccumulate
    MPI_Fetch_and_op
    MPI_Compare_and_swap
)

IF (NOT ${ALL_FEATURE_TESTS} AND ${MPI_C_VERSION} VERSION_GREATER_EQUAL "3")
    foreach (func ${mpi3_rma_functions})
        add_mpi_function_exists_pp(${func})
    endforeach (func)
ELSE()
    FOREACH (new_func ${mpi3_rma_functions})
        check_mpi_function_exists_pp(${new_func})
    ENDFOREACH ()
ENDIF()


# MPI 4
SET (new_mpi_40_functions
    MPI_Allgather_init
    MPI_Allgatherv_init
    MPI_Allreduce_init
    MPI_Alltoall_init
    MPI_Alltoallv_init
    MPI_Alltoallw_init
    MPI_Barrier_init
    MPI_Bcast_init
    MPI_Comm_create_from_group
    MPI_Comm_idup_with_info
    MPI_Exscan_init
    MPI_Gather_init
    MPI_Gatherv_init
    MPI_Group_from_session_pset
    MPI_Info_get_string
    MPI_Intercomm_create_from_groups
    MPI_Isendrecv
    MPI_Isendrecv_replace
    MPI_Neighbor_allgather_init
    MPI_Neighbor_allgatherv_init
    MPI_Neighbor_alltoall_init
    MPI_Neighbor_alltoallv_init
    MPI_Neighbor_alltoallw_init
    MPI_Parrived
    MPI_Pready
    MPI_Pready_list
    MPI_Pready_range
    MPI_Precv_init
    MPI_Psend_init
    MPI_Reduce_init
    MPI_Reduce_scatter_block_init
    MPI_Reduce_scatter_init
    MPI_Scan_init
    MPI_Scatter_init
    MPI_Scatterv_init
    MPI_Session_create_errhandler
    MPI_Session_call_errhandler
    MPI_Session_finalize
    MPI_Session_get_info
    MPI_Session_get_nth_pset
    MPI_Session_get_num_psets
    MPI_Session_get_pset_info
    MPI_Session_init
    MPI_Session_get_errhandler
    MPI_Session_set_errhandler
    MPI_Info_create_env
)


IF (NOT ${ALL_FEATURE_TESTS} AND ${MPI_C_VERSION} VERSION_GREATER_EQUAL "4")
    foreach (func ${new_mpi_40_functions})
        add_mpi_function_exists_pp(${func})
    endforeach (func)
ELSE()
    FOREACH (new_func ${new_mpi_40_functions})
        check_mpi_function_exists_pp(${new_func})
    ENDFOREACH ()
ENDIF()


set(Removed40Functions
    MPI_Info_get
    MPI_Info_get_valuelen
)

foreach (symbol ${Removed40Symbols})
    check_mpi_symbol_is_rvalue(${symbol} HAVE_${symbol})
endforeach()

##Extra types and additions for const correctness (MPI-3)
IF (HAVE_MPI_NO_CONST_CORRECTNESS)
    SET (CONSTABLE_VOIDP_TYPE "void*" CACHE INTERNAL "Type to use for void* that can be const void* in newer MPIs.")
    SET (CONSTABLE_INTP_TYPE "int*" CACHE INTERNAL "Type to use for int* that can be const int [] in newer MPIs.")
    SET (CONSTABLE_INTP_ADDITION "" CACHE INTERNAL "Addition to CONSTABLE_INTP_TYPE.")
    SET (CONSTABLE_DATATYPEP_TYPE "MPI_Datatype*" CACHE INTERNAL "Type to use for MPI_Datatype* that can be const MPI_Datatype [] in newer MPIs.")
    SET (CONSTABLE_DATATYPEP_ADDITION "" CACHE INTERNAL "Addition to CONSTABLE_DATATYPEP_TYPE.")
    SET (CONSTABLE_SINGLE_STATUSP "MPI_Status*" CACHE INTERNAL "Type to use for MPI_Status* that can be const MPI_Status* in newer MPIs.")
    SET (CONSTABLE_AINTP_TYPE "MPI_Aint*" CACHE INTERNAL "Type to use for MPI_Aint* that can be const MPI_Aint [] in newer MPIs.")
    SET (CONSTABLE_AINTP_ADDITION "" CACHE INTERNAL "Addition to CONSTABLE_AINTP_TYPE.")
ELSE ()
    SET (CONSTABLE_VOIDP_TYPE "const void*" CACHE INTERNAL "Type to use void* that can be const void* in newer MPIs.")
    SET (CONSTABLE_INTP_TYPE "const int" CACHE INTERNAL "Type to use for int* that can be const int [] in newer MPIs.")
    SET (CONSTABLE_INTP_ADDITION "typeAfterArg=\"[]\"" CACHE INTERNAL "Addition to CONSTABLE_INTP_TYPE.")
    SET (CONSTABLE_DATATYPEP_TYPE "const MPI_Datatype" CACHE INTERNAL "Type to use for MPI_Datatype* that can be const MPI_Datatype [] in newer MPIs.")
    SET (CONSTABLE_DATATYPEP_ADDITION "typeAfterArg=\"[]\"" CACHE INTERNAL "Addition to CONSTABLE_DATATYPEP_TYPE.")
    SET (CONSTABLE_SINGLE_STATUSP "const MPI_Status*" CACHE INTERNAL "Type to use for MPI_Status* that can be const MPI_Status* in newer MPIs.")
    SET (CONSTABLE_AINTP_TYPE "const MPI_Aint" CACHE INTERNAL "Type to use for MPI_Aint* that can be const MPI_Aint [] in newer MPIs.")
    SET (CONSTABLE_AINTP_ADDITION "typeAfterArg=\"[]\"" CACHE INTERNAL "Addition to CONSTABLE_AINTP_TYPE.")
ENDIF ()

#Special handling for MPI_Address
IF (HAVE_MPI_ADDRESS_CONST_CORRECT)
    SET (MPI_ADDRESS_CONSTABLE_VOIDP_TYPE "const void*" CACHE INTERNAL "Type to use void* that can be const void* in newer MPIs.")
ELSE ()
    SET (MPI_ADDRESS_CONSTABLE_VOIDP_TYPE "void*" CACHE INTERNAL "Type to use for void* that can be const void* in newer MPIs.")
ENDIF ()

#Special handling for MPI_Psend_init
IF (HAVE_MPI_PSEND_INIT_CONST_CORRECT)
    SET (MPI_PSEND_INIT_CONSTABLE_VOIDP_TYPE "const void*" CACHE INTERNAL "Type to use void* that can be const void* in newer MPIs.")
ELSE ()
    SET (MPI_PSEND_INIT_CONSTABLE_VOIDP_TYPE "void*" CACHE INTERNAL "Type to use for void* that can be const void* in newer MPIs.")
ENDIF ()

#Special handling for MPI_Psend_init
IF (HAVE_MPI_PREADY_LIST_CONST_CORRECT)
    SET (MPI_PREADY_LIST_CONSTABLE_INT_TYPE "const int" CACHE INTERNAL "Type to use void* that can be const void* in newer MPIs.")
ELSE ()
    SET (MPI_PREADY_LIST_CONSTABLE_INT_TYPE "int" CACHE INTERNAL "Type to use for void* that can be const void* in newer MPIs.")
ENDIF ()

#Special handling for MPI_Parrived
IF (HAVE_MPI_PARRIVED_PARTITION_CORRECT)
    SET (MPI_PARRIVED_PARTITION_TYPE "int" CACHE INTERNAL "Type to use for partition argument in MPI_Parrived.")
ELSE ()
    SET (MPI_PARRIVED_PARTITION_TYPE "MPI_Count" CACHE INTERNAL "Type to use for partition argument in MPI_Parrived.")
ENDIF ()

#Special handling for MPI_Type_hindexed
IF (HAVE_MPI_TYPE_HINDEXED_CONST_CORRECT)
    SET (MPI_TYPE_HINDEXED_CONSTABLE_INTP_TYPE "const int" CACHE INTERNAL "Type to use for int* that can be const int [] in newer MPIs.")
    SET (MPI_TYPE_HINDEXED_CONSTABLE_INTP_ADDITION "typeAfterArg=\"[]\"" CACHE INTERNAL "Addition to CONSTABLE_INTP_TYPE.")
    SET (MPI_TYPE_HINDEXED_CONSTABLE_AINTP_TYPE "const MPI_Aint" CACHE INTERNAL "Type to use for MPI_Aint* that can be const MPI_Aint [] in newer MPIs.")
    SET (MPI_TYPE_HINDEXED_CONSTABLE_AINTP_ADDITION "typeAfterArg=\"[]\"" CACHE INTERNAL "Addition to CONSTABLE_AINTP_TYPE.")
ELSE ()
    SET (MPI_TYPE_HINDEXED_CONSTABLE_INTP_TYPE "int*" CACHE INTERNAL "Type to use for int* that can be const int [] in newer MPIs.")
    SET (MPI_TYPE_HINDEXED_CONSTABLE_INTP_ADDITION "" CACHE INTERNAL "Addition to CONSTABLE_INTP_TYPE.")
    SET (MPI_TYPE_HINDEXED_CONSTABLE_AINTP_TYPE "MPI_Aint*" CACHE INTERNAL "Type to use for MPI_Aint* that can be const MPI_Aint [] in newer MPIs.")
    SET (MPI_TYPE_HINDEXED_CONSTABLE_AINTP_ADDITION "" CACHE INTERNAL "Addition to CONSTABLE_AINTP_TYPE.")
ENDIF ()

#Special handling for MPI_Type_struct
IF (HAVE_MPI_TYPE_STRUCT_CONST_CORRECT)
    SET (MPI_TYPE_STRUCT_CONSTABLE_INTP_TYPE "const int" CACHE INTERNAL "Type to use for int* that can be const int [] in newer MPIs.")
    SET (MPI_TYPE_STRUCT_CONSTABLE_INTP_ADDITION "typeAfterArg=\"[]\"" CACHE INTERNAL "Addition to CONSTABLE_INTP_TYPE.")
    SET (MPI_TYPE_STRUCT_CONSTABLE_AINTP_TYPE "const MPI_Aint" CACHE INTERNAL "Type to use for MPI_Aint* that can be const MPI_Aint [] in newer MPIs.")
    SET (MPI_TYPE_STRUCT_CONSTABLE_AINTP_ADDITION "typeAfterArg=\"[]\"" CACHE INTERNAL "Addition to CONSTABLE_AINTP_TYPE.")
    SET (MPI_TYPE_STRUCT_CONSTABLE_DATATYPEP_TYPE "const MPI_Datatype" CACHE INTERNAL "Type to use for MPI_Datatype* that can be const MPI_Datatype [] in newer MPIs.")
    SET (MPI_TYPE_STRUCT_CONSTABLE_DATATYPEP_ADDITION "typeAfterArg=\"[]\"" CACHE INTERNAL "Addition to CONSTABLE_DATATYPEP_TYPE.")
ELSE ()
    SET (MPI_TYPE_STRUCT_CONSTABLE_INTP_TYPE "int*" CACHE INTERNAL "Type to use for int* that can be const int [] in newer MPIs.")
    SET (MPI_TYPE_STRUCT_CONSTABLE_INTP_ADDITION "" CACHE INTERNAL "Addition to CONSTABLE_INTP_TYPE.")
    SET (MPI_TYPE_STRUCT_CONSTABLE_AINTP_TYPE "MPI_Aint*" CACHE INTERNAL "Type to use for MPI_Aint* that can be const MPI_Aint [] in newer MPIs.")
    SET (MPI_TYPE_STRUCT_CONSTABLE_AINTP_ADDITION "" CACHE INTERNAL "Addition to CONSTABLE_AINTP_TYPE.")
    SET (MPI_TYPE_STRUCT_CONSTABLE_DATATYPEP_TYPE "MPI_Datatype*" CACHE INTERNAL "Type to use for MPI_Datatype* that can be const MPI_Datatype [] in newer MPIs.")
    SET (MPI_TYPE_STRUCT_CONSTABLE_DATATYPEP_ADDITION "" CACHE INTERNAL "Addition to CONSTABLE_DATATYPEP_TYPE.")
ENDIF ()


SET (LOGICAL_TS_TYPE "unsigned long" CACHE INTERNAL "Type to use for MUST's logical time stamps.")
