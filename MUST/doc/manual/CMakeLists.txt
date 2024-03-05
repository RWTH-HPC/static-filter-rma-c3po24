# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause
#
# @file CMakeLists.cmake
#       CMake file for the doc/manual.
#
# @author Tobias Hilbrich
# @date 30.08.2011

find_package(LATEX QUIET COMPONENTS PDFLATEX)

if(LATEX_FOUND)
    add_custom_target(manual
            # Run twice
            COMMAND ${PDFLATEX_COMPILER} manual.tex
            COMMAND ${PDFLATEX_COMPILER} manual.tex
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            )
endif()

#Install manual
INSTALL(FILES manual.pdf DESTINATION share/doc/must)
