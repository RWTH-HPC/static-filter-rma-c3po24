/*
 * Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*
 * Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file Command.cpp
 *
 *  @date 03.06.23
 *  @author Sebastian Grabowski
 */

#include "Command.hpp"

#include <cstdlib>
#include <string>
#include <cstring>
#include <clocale>

#include "PrefixedOstream.hpp"

namespace must
{

void system_checked(const std::string& command)
{
    int status = std::system(command.c_str());
    if (status == -1) {
        must::cerr << "Could not execute '" << command
                   << "'. Reason: " << strerror_l(errno, uselocale((locale_t) nullptr)) << ".\n"
                   << std::flush;
    }
    if (WIFEXITED(status) && WEXITSTATUS(status) != 0) {
        must::cerr << "The command '" << command << "' exited with non-zero status "
                   << WEXITSTATUS(status) << ".\n"
                   << std::flush;
    }
    if (WIFSIGNALED(status)) {
        must::cerr << "The command '" << command << "' received the signal " << WTERMSIG(status)
                   << ".\n"
                   << std::flush;
    }
}

} // namespace must
