# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

# @file CMakeLists.cmake
#       CMake file for the specifications directory.
#
# @author Tobias Hilbrich
# @date 03.01.2011

SET (ArgId_TYPE "int" CACHE INTERNAL "Type of the argument ids.")


# Specific analyses and functions may be enabled dynamically by defining a
# specific attribute in the XML node's definition. Therefore, the following code
# will convert a set of CMake variables to simple 'YES' / 'NO' values depending
# on their boolean value.
FOREACH (VAR "OMPT_FOUND")
    IF (${VAR})
        SET("SPEC_${VAR}" "yes")
    ELSE ()
        SET("SPEC_${VAR}" "no")
    ENDIF ()
ENDFOREACH ()


# Configure and install all specifications.
#
# NOTE: Specification files will be searched dynamically to support excluding
#       specific specifications by simply removing its file (i.e. to not ship an
#       experimental module's specification on deploy).
#
# NOTE: The xmls that are configured should have a ".in" at the end, but this
#       will disable editor support for them, so we didn't add this extension.
FILE(GLOB SPECIFICATIONS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
          "*.xml"
          "optional/*.xml")
FOREACH (SPEC ${SPECIFICATIONS})
    CONFIGURE_FILE(${SPEC} ${SPEC} @ONLY)
    MUST_INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/${SPEC}
                 DESTINATION specifications)
ENDFOREACH ()
