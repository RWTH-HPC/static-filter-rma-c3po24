# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for RequestChecks test.
#
# @author Tobias Hilbrich,Mathias Korepkat
# @date 5.04.2011
#

##################
#Recurse into sub directories
##Not needed

###################
#Add test cases

##Mrecv
addProcessLocalTest ("UnsupportedMrecv" "Mrecv.c" 2
    ""
    "Error;ERROR;Warning;WARNING"
    )
