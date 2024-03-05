/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "PrefixedOstream.hpp"

namespace must
{

// It should be safe to use std::cout, etc. here, because
// std::ios_base::Init is already included through the iostream
// header. This ensures that they are already initialized.
/** Prefixing drop-in replacement for std::cout */
must::PrefixedOstream cout{MUST_STDOUT_PREFIX_RUNTIME, std::cout};
/** Prefixing drop-in replacement for std::cerr */
must::PrefixedOstream cerr{MUST_STDOUT_PREFIX_RUNTIME, std::cerr};
/** Prefixing drop-in replacement for std::clog */
must::PrefixedOstream clog{MUST_STDOUT_PREFIX_RUNTIME, std::clog};

} // namespace must
