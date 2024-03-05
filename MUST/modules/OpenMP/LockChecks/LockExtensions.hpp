/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MUST_LOCKEXTENSIONS_HPP
#define MUST_LOCKEXTENSIONS_HPP

#include "omp-tools.h"
#include <map>
#include <mutex>

namespace must
{
namespace openmp
{
class LockExtensions
{
    using OmpLock = ompt_wait_id_t;

    std::mutex myMutexesMutex;
    using mutex_map = std::map<OmpLock, std::mutex>;
    mutex_map myMutexes;

    auto get_or_default(OmpLock lock) -> mutex_map::mapped_type&
    {
        std::lock_guard<std::mutex> guard{myMutexesMutex};
        return myMutexes[lock];
    }

    auto get(OmpLock lock) -> mutex_map::mapped_type&
    {
        std::lock_guard<std::mutex> guard{myMutexesMutex};
        return myMutexes.at(lock);
    }

  public:
    auto begin_extension(OmpLock lock) -> void { get_or_default(lock).lock(); };

    auto end_extension(OmpLock lock) -> void { get(lock).unlock(); };
};
} // namespace openmp
} // namespace must

#endif // MUST_LOCKEXTENSIONS_HPP
