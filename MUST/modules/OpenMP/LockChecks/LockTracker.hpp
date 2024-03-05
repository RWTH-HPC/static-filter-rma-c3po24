/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MUST_LOCKTRACKER_HPP
#define MUST_LOCKTRACKER_HPP

#include "BaseIds.h"
#include "omp-tools.h"
#include "safe_ptr.h"
#include <map>

namespace must
{
namespace openmp
{

class LockTracker
{
    using OmpLock = ompt_wait_id_t;
    using Owner = MustParallelId;

    sf::contfree_safe_ptr<std::map<OmpLock, Owner>> myLocks{};

  public:
    auto initialized(OmpLock lock) const -> bool
    {
        const auto sptr = slock_safe_ptr(myLocks);
        return sptr->find(lock) != sptr->cend();
    };

    auto owner(OmpLock lock) const -> Owner
    {
        const auto sptr = slock_safe_ptr(myLocks);
        return sptr->find(lock)->second;
    };

    auto initialize(OmpLock lock) -> void
    {
        auto xptr = xlock_safe_ptr(myLocks);
        (*xptr)[lock] = 0;
    };

    auto destroy(OmpLock lock) -> void
    {
        auto xptr = xlock_safe_ptr(myLocks);
        xptr->erase(lock);
    };

    auto acquire(OmpLock lock, Owner owner) -> void
    {
        auto xptr = xlock_safe_ptr(myLocks);
        (*xptr)[lock] = owner;
    };

    auto release(OmpLock lock) -> void
    {
        auto xptr = xlock_safe_ptr(myLocks);
        (*xptr)[lock] = 0;
    };
};

} // namespace openmp
} // namespace must

#endif // MUST_LOCKTRACKER_HPP
