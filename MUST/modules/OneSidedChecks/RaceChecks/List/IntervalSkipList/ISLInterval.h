/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file ISLInterval.h
 *
 *  @date 19.06.2023
 *  @author Sem Klauke
 */
#ifndef ISLInterval_h
#define ISLInterval_h

#include "MemAccess.h"
#include <memory>
#include <vector>

namespace ISL
{

class ISLInterval
{
  public:
    MemAddress left{};  // lowest memaddress in interval
    MemAddress right{}; // highest memaddress in interval

    ~ISLInterval();
    /* NO COPY!
     * ISLInterval need to stay unique since we do idenity checks on it in the data structure.
     * (e.g. there can only be one tail)
     */
    ISLInterval(ISLInterval const& intvl) = delete;
    ISLInterval();
    ISLInterval(MemAddress left, MemAddress right);

    /**
     * Contains check.
     * Check if input is fully contained is this interval [left,right]
     * True if [input.left, input.right] \subseteq [this->left, this->right]
     */
    bool contains(const ISLInterval& intvl) const;
    bool contains(const MemAddress& addr) const;
    bool contains(const MemAddress& left, const MemAddress& right) const;

    /**
     * Overlap check.
     * Check if input is overlapping with this interval [left,right]
     * True if [input.left, input.right] \cap [this->left, this->right] \neq \emptyset
     */
    bool overlaps(const ISLInterval& intvl) const;
    bool overlaps(const MemAddress& left, const MemAddress& right) const;

    /**
     * Comparison operator
     * @return true if this->left < other.left
     */
    bool operator<(const ISLInterval& other) const;

    /**
     * Add data (MemAccess) to this interval.
     * The pointer is now managed by this Interval class.
     * On destruction this pointer gets freed.
     *
     * @param access memory access data to this memory interval
     */
    void addMemAccess(MemAccess* access);

    /**
     * Getter for data (memory accesses) saved in this interval.
     *
     * @param out vector to put pointers to the MemAccess's saved in here
     * @param onlyWrite whether to only return MemAccess that are writes
     */
    void getAccesses(std::vector<const MemAccess*>& out, bool onlyWrite = false) const;

    /**
     * Getter for data (memory accesses) saved in this interval,
     * that conflicts with the given `access`.
     * This is a constraint on getAccesses()
     *
     * @param out vector to put pointers to the MemAccess's saved in here
     * @param access only add MemAccess to `out` that conflicts with this access
     * @param onlyWrite whether to only return MemAccess that are writes
     */
    void getConflicts(
        std::vector<const MemAccess*>& out,
        const MemAccess& access,
        bool onlyWrite = false) const;

    /**
     * @see ISLInterval::getConflicts
     * @overload 
     * (see above) with non const output
     */
    void getConflicts(std::vector<MemAccess*>& out, const MemAccess& access, bool onlyWrite = false)
        const;

  private:
    /* seperate storage of write and read Access if we want to conflict check
       with other access that is an read, we only need to check against the write
       stored here*/
    std::vector<std::unique_ptr<MemAccess>> writeAccesses;
    std::vector<std::unique_ptr<MemAccess>> readAccesses;
};

} /* end namespace ISL */

#endif