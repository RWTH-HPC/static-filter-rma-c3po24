/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file IntervalSkiplist.h
 *
 *  @date 19.06.2023
 *  @author Sem Klauke
 */
#ifndef ISL_h
#define ISL_h

#include "MemAccess.h"
#include "ISLInterval.h"
#include "ISLNode.h"
#include <memory>
#include <random>
#include <vector>

namespace ISL
{

class IntervalSkiplist
{
  public:
    ~IntervalSkiplist();
    IntervalSkiplist(IntervalSkiplist const& isl);
    IntervalSkiplist();
    /**
     * Constructor
     *
     * @param p probability of adding another level to a new node. p \in (0,1)
          -> probability that a new node has level x is x^p
     */
    IntervalSkiplist(float p);

    /**
     * Insert MemAccess into skiplist.
     * Memory is now managed by ISL. On deconstruct this pointer is freed
     *
     * @param access MemAccess pointer to add to ISL
     */
    void insertMemAccess(MemAccess* access);

    /**
     * Add ISLInterval to ISL.
     *
     * @param intvl interval to add to ISL (shared_ptr since ISLInterval is non-copy)
     */
    void insertInterval(std::shared_ptr<ISLInterval> intvl);

    /**
     * Find interval instances that contain `key`
     * @see ISLInterval:contains
     *
     * @param key search key
     * @return raw pointers to intervals in the ISL that contain the search key
     */
    std::set<const ISLInterval*> findIntervals(MemAddress key) const;

    /**
     * Find interval instances that overlap [left, right]
     * @see ISLInterval:overlaps
     *
     * @param left lowest address of search interval
     * @param right highest address of search interval
     * @return raw pointers to intervals in the ISL that overlap the search interval
     */
    std::set<const ISLInterval*> findIntervals(MemAddress left, MemAddress right) const;

    /**
     * Clears the skiplist.
     * All nodes, intervals (markers) and the data they hold (MemAccesses) gets freed.
     */
    void clear();

    // =========================================
    // METHODS FOR DATA RACE DETECTION
    // =========================================

    /**
     * Find mem accesses stored in the ISL that
     *   1. have an overlapping mem access with the input [left, right]
     *
     * @param left lowest address of search interval
     * @param left highest address of search interval
     * @return const pointers into the ISL of the conflicting mem accesses
     */
    std::vector<const MemAccess*>
    findConflictingMemAccesses(MemAddress left, MemAddress right) const;

    /**
     * Find mem accesses stored in the ISL that
     *   1. have an overlapping mem access with the input [access.left, access.right]
     *   2. produce an data race with the given Memaccess `access` @see MemAccess:conflicts
     *
     * @overload
     * @param access mem access to check against data races in the ISL
     * @return const pointers into the ISL of mem accesses that produce a data race
     */
    std::vector<const MemAccess*> findConflictingMemAccesses(const MemAccess& access) const;

    /**
     * @see IntervalSkiplist::findConflictingMemAccesses
     * Non const pointer return
     *
     * @param access mem access to check against data races in the ISL
     * @return pointers into the ISL of mem accesses that produce a data race
     */
    std::vector<MemAccess*> findConflictingMemAccesses_not_const(const MemAccess& access);

    /**
     * Combination of insertMemAccess and findConflictingMemAccess. For convenience.
     * First check for data races with the given MemAcess, then inserts it into the ISL
     *
     * @see IntervalSkiplist::insertMemAccess IntervalSkiplist::findConfflictingMemAccess
     * @param access mem access to check for data race and insert
     * @param out output mem accesses that produce a race with `access`
     * @return true if there was a race (out.size() != 0)
     */
    bool insertAndCheckConflicts(MemAccess* access, std::vector<const MemAccess*>& out);

    /**
     * Non const output variant of @see IntervalSkiplist::insertAndCheckConflicts
     *
     * @overload
     */
    bool insertAndCheckConflicts(MemAccess* access, std::vector<MemAccess*>& out);

    /**
     * Prints ASCII version of the ISL to stdout.
     * The markers are printed as the pointer addresses of the ISLIntervals
     * that represent them.
     */
    void print() const;

  private:
    float p; // probability that a new node has level x is x^p
    std::shared_ptr<ISLNode> header;
    std::shared_ptr<ISLNode> tail;
    int maxLevel; // level of the highest node(s)
    std::mt19937 randomGen;

    /**
     * Searches for interval [left, right] in the ISL.
     * If it doesn't exist, this creates and inserts it
     */
    ISLInterval* getInterval(MemAddress left, MemAddress right);

    // see original paper DOI:10.1007/BFb0028258
    /**
     * Removes marker `intvl` from level `lvl` between the nodes `start` and `end`
     *
     * @param intvl the marker (interval) to remove from the edges
     * @param lvl from which leval on each node should the marker be removed from
     * @param start start the remove here
     * @param end end the remove if you reach this noe
     */
    void removeMarkerFromLvl(
        std::shared_ptr<ISLInterval> const intvl,
        int lvl,
        ISLNode& start,
        ISLNode& end);

    /**
     * Update the ISL to satisfy the marker invariant after insert (see paper)
     *
     * @param node the node that was just inserted
     * @param updated the nodes we need to update after the insert
     */
    void adjustMarkerOnInsert(ISLNode& node, std::vector<ISLNode*>& updated);

    /**
     * And marker `intvl` on the path from node `left` to `right`
     *
     * @param left start node of interval
     * @param right end node of interval
     * @param intvl marker to add on path from `left` to `right`
     */
    void placeMarker(
        std::shared_ptr<ISLNode> left,
        std::shared_ptr<ISLNode> right,
        std::shared_ptr<ISLInterval> intvl);

    /**
     * Find node with search key `key`.
     * If it doesn't exists, return tail node
     *
     * @param key search key
     * @param toUpdate (output) We add which nodes need to be updated if we need to
     * insert a new node (see adjustMarkerOnInsert)
     * @return Node with key `key` or tail if it doesn't exist
     */
    std::shared_ptr<ISLNode> findNode(MemAddress key, std::vector<ISLNode*>& toUpdate) const;

    /**
     * Insert search key `key` as node into the ISL.
     * If there already is a node with this key, just return this node
     */
    std::shared_ptr<ISLNode> insertKey(MemAddress key);

    /**
     * Random level for each new node. Base on this->p
     *
     * @return level \in [0,\infty)
     **/
    int newRandomLvl();

    /**
     * Internal. Merge `source` into `target` without removing the elements from source
     */
    void mergeIntervalPathNondestructive(
        const std::set<std::shared_ptr<ISLInterval>>& source,
        std::set<const ISLInterval*>& target) const;
};

} /* end namespace ISL */

#endif