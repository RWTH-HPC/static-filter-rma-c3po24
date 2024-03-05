# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for PartitionedP2PChecks directory.
#
# @author Joachim Protze
# @date 20.09.2022
#

IF(HAVE_MPI_PSEND_INIT)
ELSE()
RETURN()
ENDIF()

##################
#Recurse into sub directories
##Not needed

###################
#Add test cases

SET(PP2PNoErrorTests 
        ParrivedNoError
        PartitionedCommNoError
        PersistentCommNoError
        PreadyListNoError
        PreadyRangeNoError
    CACHE INTERNAL "All PartitionedP2PChecks no error tests"
)


SET(PP2PErrorTests 
        Deadlock-MissingPreadyError
        Deadlock-PartitionedRequestWaitAllError
        Deadlock-WrongOrderPsendPrecvError
        DoubleActivation-PreadyListDuplicatesInArrayError
        DoubleActivation-PreadyListOnActivePartitionError
        DoubleActivation-PreadyOnActivePartitionError
        DoubleActivation-PreadyRangeOnActivePartitionError
        InvalidArg-NegativePartitionCountError
        InvalidArg-ParrivedWithoutPartitionedRecvRequestError
        InvalidArg-PreadyListOutOfRangeError
        InvalidArg-PreadyOutOfRangeError
        InvalidArg-PreadyRangeOutOfRangeError
        InvalidArg-PreadyWithoutPartitionedSendRequestError
        InvalidArg-WildcardInPrecvInitError
        LocalBufferRace-Parrived
        LocalBufferRace-Pready
    CACHE INTERNAL "All PartitionedP2PChecks error tests"
)

##No Error
FOREACH(test ${PP2PNoErrorTests})
addProcessLocalTest ("${test}" "${test}.cpp" 2 
    ""
    "Error;ERROR;Warning;WARNING"
    )
ENDFOREACH()
    
## Error:
FOREACH(test ${PP2PErrorTests})
addProcessLocalTest ("${test}" "${test}.cpp" 2 
    "[MUST-REPORT].*Error"
    ""
    )
ENDFOREACH()
