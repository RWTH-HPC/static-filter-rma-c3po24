/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file MustOutputdir.h
 *       @see MustOutputdir.
 *
 *  @date 17.08.2011
 *  @author Mathias Korepkat
 */

#ifndef MUSTOUTPUTDIR_H
#define MUSTOUTPUTDIR_H

#include "mpi.h"
#include <stdlib.h>
#include <stdint.h>
#include <string>
#include <fstream>

// Replacement for getBaseOutputDir()
std::string const& get_base_output_dir();
// Replacement for MUST_OUTPUT_DIR macro (no more invoking UB by calling .c_str() on a temporary)
std::string must_output_dir_with_file(char const* file_name);

// Opens a file `file_name` in the MUST_Output directory
// USE THIS IF YOU WANT TO OPEN A FILE IN THE OUTPUT DIRECTORY,
// OTHERWISE THE FILE WONT BE TRACKED
std::ofstream must_output_open_file(char const* file_name);

// Replacement for MUST_DIR_CHECK
void must_ensure_dir_exists(char const* path);
// Replacement for MUST_OUTPUT_DIR_CHECK
void must_ensure_output_dir_exists();

#define MUST_OUTPUT_REDIR "../"

const char* const MUST_STDOUT_PREFIX_REPORT = "[MUST-REPORT] ";
const char* const MUST_STDOUT_PREFIX_RUNTIME = "[MUST-RUNTIME] ";

#endif /*MUSTOUTPUTDIR_H*/
