/*
 * Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file Command.hpp
 *
 *  @date 03.06.23
 *  @author Sebastian Grabowski
 */

#ifndef MUST_COMMAND_HPP
#define MUST_COMMAND_HPP

#include <string>

namespace must
{

/**
 * Call system(const char*) and emit error messages on must::cerr if the command was not successful.
 *
 * It checks for and reports:
 *  - the shell not being able to execute the command
 *  - non-zero return values from the command
 *  - termination of the child process by a signal
 *
 * @param command The command string passed to the call.
 */
void system_checked(const std::string& command);

} // namespace must

#endif // MUST_COMMAND_HPP
