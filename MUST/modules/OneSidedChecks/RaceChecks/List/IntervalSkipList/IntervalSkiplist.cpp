/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file IntervalSkiplist.cpp
 *
 *  @date 19.06.2023
 *  @author Sem Klauke
 */
#include "IntervalSkiplist.h"
#include "ISLInterval.h"
#include "ISLNode.h"
#include "MemAccess.h"
#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <iterator>
#include <memory>
#include <vector>
#include <set>
#include <random>
#include <sstream>
#include <iostream>
#include <string>

namespace ISL
{

//=============================
// Destructor.
//=============================
// since pointer is a smart pointer, no manual cleanu necessary
// TODO: verify that, e.g. by AddressSanitizer
IntervalSkiplist::~IntervalSkiplist() = default;

//=============================
// Copy Constructor.
//=============================
IntervalSkiplist::IntervalSkiplist(IntervalSkiplist const& isl) = default;

//=============================
// Constructors.
//=============================
IntervalSkiplist::IntervalSkiplist() : IntervalSkiplist{0.5} {}

IntervalSkiplist::IntervalSkiplist(float p)
    : p{p}, header{std::make_shared<ISLNode>(0)}, tail{std::make_shared<ISLNode>(0)}, maxLevel{-1}
{
    header->setForward(0, tail);
    // init random device
#ifdef MUST_DEBUG
    randomGen = std::mt19937(55); // static seed for debugging
#else
    randomGen = std::mt19937(std::random_device{}());
    randomGen.discard(1000);
#endif
}

//=============================
// findIntervals
//=============================
std::set<const ISLInterval*> IntervalSkiplist::findIntervals(MemAddress key) const
{
    auto x = header;
    std::set<const ISLInterval*> path{};
    for (int i = maxLevel; i > 0; --i) {
        while (x->forward[i] != tail && x->forward[i]->key < key) {
            x = x->forward[i];
        }
        mergeIntervalPathNondestructive(x->markers[i], path);
    }
    while (x->forward[0] != tail && x->forward[0]->key < key) {
        x = x->forward[0];
    }
    if (x->forward[0] == tail || x->forward[0]->key != key) {
        mergeIntervalPathNondestructive(x->markers[0], path);
    } else {
        mergeIntervalPathNondestructive(x->forward[0]->owners, path);
    }

    return path;
}

std::set<const ISLInterval*>
IntervalSkiplist::findIntervals(MemAddress left, MemAddress right) const
{
    auto x = header;
    std::shared_ptr<ISLNode> leftNode = nullptr;
    std::set<const ISLInterval*> path{};

    for (int i = maxLevel; i >= 0; --i) {
        while (x->forward[i] != tail && x->forward[i]->key < left) {
            x = x->forward[i];
        }
        leftNode = x;
        while (x->forward[i] != tail && x->forward[i]->key <= right) {
            mergeIntervalPathNondestructive(x->markers[i], path);
            x = x->forward[i];
            mergeIntervalPathNondestructive(x->owners, path);
        }
        mergeIntervalPathNondestructive(x->markers[i], path);
        x = leftNode;
    }

    return path;
}

//=============================
// insertMemAccess
//=============================
void IntervalSkiplist::insertMemAccess(MemAccess* access)
{
    ISLInterval* intvl = getInterval(access->left, access->right);
    intvl->addMemAccess(access);
}

//=============================
// findConflictingMemAccesses
//=============================
std::vector<const MemAccess*>
IntervalSkiplist::findConflictingMemAccesses(MemAddress left, MemAddress right) const
{
    std::vector<const MemAccess*> confAccesses{};
    for (auto m : findIntervals(left, right)) {
        m->getAccesses(confAccesses);
    }
    return confAccesses;
}

std::vector<const MemAccess*>
IntervalSkiplist::findConflictingMemAccesses(const MemAccess& access) const
{
    std::vector<const MemAccess*> confAccesses{};
    bool onlyWrite = !access.isWrite;

    for (auto m : findIntervals(access.left, access.right)) {
        m->getConflicts(confAccesses, access, onlyWrite);
    }

    return confAccesses;
}

// non const return
std::vector<MemAccess*>
IntervalSkiplist::findConflictingMemAccesses_not_const(const MemAccess& access)
{
    std::vector<MemAccess*> confAccesses{};
    bool onlyWrite = !access.isWrite;

    for (auto m : findIntervals(access.left, access.right)) {
        m->getConflicts(confAccesses, access, onlyWrite);
    }

    return confAccesses;
}

//=============================
// insertAndCheckConflicts
//=============================
// returns true on conflict
bool IntervalSkiplist::insertAndCheckConflicts(
    MemAccess* access,
    std::vector<const MemAccess*>& out)
{
    out = findConflictingMemAccesses(*access);
    insertMemAccess(access);
    return out.size() != 0;
}

// non const output
// returns true on conflict
bool IntervalSkiplist::insertAndCheckConflicts(MemAccess* access, std::vector<MemAccess*>& out)
{
    out = findConflictingMemAccesses_not_const(*access);
    insertMemAccess(access);
    return out.size() != 0;
}

//=============================
// insertInterval
//=============================
void IntervalSkiplist::insertInterval(std::shared_ptr<ISLInterval> intvl)
{
    auto l = insertKey(intvl->left);
    auto r = insertKey(intvl->right);
    l->owners.insert(intvl);

    placeMarker(l, r, intvl);
}


//=============================
// placeMarker
//=============================
void IntervalSkiplist::placeMarker(
    std::shared_ptr<ISLNode> left,
    std::shared_ptr<ISLNode> right,
    std::shared_ptr<ISLInterval> intvl)
{
    auto x = left;
    int i = 0;
    while (x->forward[i] != tail && intvl->contains(x->key, x->forward[i]->key)) {
        auto lvl = x->getLevel();
        while (i != (lvl - 1) && x->forward[i + 1] != tail &&
               intvl->contains(x->key, x->forward[i + 1]->key)) {
            i += 1;
        }
        if (x->forward[i] != tail) {
            x->markers[i].emplace(intvl);
            x = x->forward[i];
            if (intvl->contains(x->key)) {
                x->owners.emplace(intvl);
            }
        }
    }
    while (x->key != right->key) {
        while (i != 0 && (x->forward[i] == tail || !intvl->contains(x->key, x->forward[i]->key))) {
            i -= 1;
        }
        x->markers[i].emplace(intvl);
        x = x->forward[i];
        if (intvl->contains(x->key)) {
            x->owners.emplace(intvl);
        }
    }
}

//=============================
// findNode
//=============================
std::shared_ptr<ISLNode>
IntervalSkiplist::findNode(MemAddress key, std::vector<ISLNode*>& toUpdate) const
{
    auto x = header;
    for (int i = maxLevel; i >= 0; --i) {
        while (x->forward[i] != tail && x->forward[i]->key < key) {
            x = x->forward[i];
        }
        toUpdate[i] = x.get();
    }
    x = x->forward[0];

    return x;
}

//=============================
// insertKey
//=============================
std::shared_ptr<ISLNode> IntervalSkiplist::insertKey(MemAddress key)
{
    std::vector<ISLNode*> toUpdate(maxLevel + 1);
    auto node = findNode(key, toUpdate);
    if (node == tail || node->key != key) {
        // node wasn't found, insert new
        int lvl = newRandomLvl();
        // update maxLevel and insert new pointers in header if necessary
        if (lvl > maxLevel) {
            for (auto i = header->getLevel(); i <= lvl; ++i) {
                header->forward.push_back(tail);
            }
            for (auto i = maxLevel + 1; i <= lvl; ++i) {
                header->markers.emplace_back();
                toUpdate.push_back(header.get());
            }
            maxLevel = lvl;
        }
        // WHY THE F*** DOES THIS CRASH THE STACK BUT THAT BELOW DOESNT
        // auto newnode = std::make_shared<ISLNode>(key, lvl);
        auto newnode = std::shared_ptr<ISLNode>(new ISLNode(key, lvl));
        for (auto i = 0; i <= lvl; ++i) {
            newnode->markers.emplace_back();
            newnode->forward.push_back(toUpdate[i]->forward[i]);
            toUpdate[i]->forward[i] = newnode;
        }
        adjustMarkerOnInsert(*newnode, toUpdate);
        return newnode;
    } else
        return node;
}

//=============================
// adjustMarkerOnInsert
//=============================
void IntervalSkiplist::adjustMarkerOnInsert(ISLNode& node, std::vector<ISLNode*>& updated)
{
    // re-establish list invariant
    int lvl = node.getLevel();

    // Phase 1: place marker on edges leading out of n
    std::set<std::shared_ptr<ISLInterval>> promoted{};
    std::set<std::shared_ptr<ISLInterval>> newPromoted{};

    int i = 0;
    while (i <= (lvl - 2) && node.forward[i + 1] != tail) {

        for (auto& m : updated[i]->markers[i]) {
            if (m->contains(node.key, node.forward[i + 1]->key)) {
                newPromoted.insert(m);
                removeMarkerFromLvl(m, i, *node.forward[i], *node.forward[i + 1]);
            } else {
                node.markers[i].insert(m);
            }
        }
        auto promotedCopy = promoted;
        for (auto& m : promotedCopy) {
            if (!m->contains(node.key, node.forward[i + 1]->key)) {
                node.markers[i].emplace(m);
                if (m->contains(node.forward[i]->key)) {
                    node.forward[i]->owners.emplace(m);
                }
                promoted.erase(m);
            } else {
                removeMarkerFromLvl(m, i, *node.forward[i], *node.forward[i + 1]);
            }
        }
        promoted.merge(newPromoted);
        newPromoted.clear();
        i += 1;
    }

    node.markers[i].clear();
    node.markers[i] = updated[i]->markers[i]; // copy
    for (auto& m : promoted) {
        node.markers[i].emplace(m);
        if (m->contains(node.forward[i]->key)) {
            node.forward[i]->owners.emplace(m);
        }
    }

    // Phase 2: adjust markers to the left of n
    promoted.clear();
    newPromoted.clear();
    i = 0;

    while (i <= (lvl - 2) && updated[i + 1] != header.get()) {
        auto markersCopy = updated[i]->markers[i];
        for (auto& m : markersCopy) {
            if (m->contains(updated[i + 1]->key, node.key)) {
                newPromoted.insert(m);
                removeMarkerFromLvl(m, i, *updated[i + 1], node);
            }
        }
        auto promotedCopy = promoted;
        for (auto& m : promotedCopy) {
            if (updated[i] != header.get() && m->contains(updated[i]->key, node.key) &&
                updated[i + 1] != header.get() && !m->contains(updated[i + 1]->key, node.key)) {
                updated[i]->markers[i].emplace(m);
                if (m->contains(updated[i]->key)) {
                    updated[i]->owners.emplace(m);
                }
                promoted.erase(m);
            } else {
                removeMarkerFromLvl(m, i, *updated[i + 1], node);
            }
        }
        promoted.merge(newPromoted);
        newPromoted.clear();
        i += 1;
    }

    for (auto& m : promoted) {
        updated[i]->markers[i].insert(m);
        if (m->contains(updated[i]->key)) {
            updated[i]->owners.emplace(m);
        }
    }

    for (int j = 0; j < lvl; ++j) {
        auto markersCopy = node.markers[j];
        node.owners.merge(markersCopy);
    }
}

//=============================
// removeMarkerFromLvl
//=============================
void IntervalSkiplist::removeMarkerFromLvl(
    std::shared_ptr<ISLInterval> const itvl,
    int lvl,
    ISLNode& start,
    ISLNode& end)
{
    auto x = &start;
    while (x != tail.get() && x != &end) {
        x->markers[lvl].erase(itvl);
        x->owners.erase(itvl);
        x = x->forward[lvl].get();
    }
    if (x != tail.get()) {
        x->owners.erase(itvl);
    }
}

//=============================
// print
//=============================
void IntervalSkiplist::print() const
{
    std::stringstream levels[maxLevel + 1];
    auto x = header;

    while (x != tail) {
        std::stringstream mark_string[maxLevel + 1];

        // node on this lvl
        for (int lvl = 0; lvl < x->getLevel(); ++lvl) {
            levels[lvl] << "->[" << std::hex << x->key << "]-";
            std::string sep = "";
            for (auto& m : x->markers[lvl]) {
                mark_string[lvl] << sep << ((std::uint64_t)m.get() % 1000);
                sep = ",";
            }
        }

        size_t max_mark_string_len = 0;
        for (std::stringstream& s : mark_string) {
            size_t len = s.str().length();
            max_mark_string_len = std::max({max_mark_string_len, len});
        }

        for (int lvl = 0; lvl < x->getLevel(); ++lvl) {
            auto l = mark_string[lvl].str().length();
            if (l < max_mark_string_len) {
                mark_string[lvl] << std::string(max_mark_string_len - l, '-');
            }
            levels[lvl] << mark_string[lvl].str();
        }

        // node is not on this lvl
        for (int lvl = x->getLevel(); lvl <= maxLevel; ++lvl) {
            auto node_key = std::stringstream() << std::hex << x->key;
            levels[lvl] << std::string(5 + (node_key.str().length()), '-');
            levels[lvl] << std::string(max_mark_string_len, '-');
        }

        x = x->forward[0];
    }

    std::string finalLevels[maxLevel + 1];
    for (int lvl = 0; lvl <= maxLevel; ++lvl) {
        levels[lvl] << "->[T]";
        finalLevels[lvl] = levels[lvl].str();
        finalLevels[lvl].erase(0, 2);
        finalLevels[lvl][1] = 'H';
    }

    for (int lvl = maxLevel + 1; lvl-- > 0;) {
        std::cout << finalLevels[lvl] << std::endl;
    }
}

//=============================
// getInterval
//=============================
ISLInterval* IntervalSkiplist::getInterval(MemAddress left, MemAddress right)
{
    auto l = insertKey(left);
    auto r = insertKey(right);
    /* see if there is an Interval on the markers of 'left'
       that has it's end exact on 'right' */
    auto intvlPresent = std::find_if(l->owners.begin(), l->owners.end(), [right](const auto& m) {
        return m->right == right;
    });
    if (intvlPresent != l->owners.end())
        return intvlPresent->get();

    /* else create new Interval */
    auto intvl = std::make_shared<ISLInterval>(left, right);
    l->owners.insert(intvl);
    placeMarker(l, r, intvl);
    return intvl.get();
}

//=============================
// mergeIntervalPathNondestructive
//=============================
void IntervalSkiplist::mergeIntervalPathNondestructive(
    const std::set<std::shared_ptr<ISLInterval>>& source,
    std::set<const ISLInterval*>& target) const
{
    for (auto& m : source) {
        target.emplace(m.get());
    }
}

//=============================
// newRandomLvl
//=============================
int IntervalSkiplist::newRandomLvl()
{
    int lvl = 0;
    std::uniform_real_distribution<float> dist(0, 1);
    while (dist(randomGen) >= p) {
        ++lvl;
    }
    return lvl;
}

//=============================
// clear 
//=============================
void IntervalSkiplist::clear()
{
    // start the deallocation chain by deleting header
    this->header = std::make_shared<ISLNode>(0);
    this->tail = std::make_shared<ISLNode>(0);
    this->maxLevel = -1;
    this->header->setForward(0, tail);
}

} /* end namespace ISL */