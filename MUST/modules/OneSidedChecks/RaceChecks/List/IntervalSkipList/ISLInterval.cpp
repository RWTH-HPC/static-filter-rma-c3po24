/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file ISLInterval.cpp
 *
 *  @date 19.06.2023
 *  @author Sem Klauke
 */
#include "ISLInterval.h"
#include "MemAccess.h"
#include <iterator>
#include <memory>
#include <numeric>
#include <vector>

namespace ISL
{

//=============================
// Destructor.
//=============================
ISLInterval::~ISLInterval() = default;

//=============================
// Constructors.
//=============================
ISLInterval::ISLInterval() = default;
ISLInterval::ISLInterval(MemAddress left, MemAddress right) : left{left}, right{right} {}

//=============================
// Contains checks
//=============================
bool ISLInterval::contains(const ISLInterval& intvl) const
{
    return intvl.left >= this->left && intvl.right <= this->right;
}

bool ISLInterval::contains(const MemAddress& addr) const
{
    return addr >= this->left && addr <= this->right;
}

bool ISLInterval::contains(const MemAddress& left, const MemAddress& right) const
{
    return left >= this->left && right <= this->right;
}

//=============================
// Overlap checks
//=============================
bool ISLInterval::overlaps(const ISLInterval& intvl) const
{
    return this->overlaps(intvl.left, intvl.right);
}

bool ISLInterval::overlaps(const MemAddress& left, const MemAddress& right) const
{
    return this->contains(left) || this->contains(right) ||
           (left <= this->left && right >= this->right);
}

//=============================
// less than operator<
//=============================
bool ISLInterval::operator<(const ISLInterval& toComp) const { return this->left < toComp.left; }

//=============================
// addMemAccess
//=============================
// IDEA: construct MemAccess inplace (give addMemaAccess the parameter for MemAccess constructuor)
void ISLInterval::addMemAccess(MemAccess* access)
{
    if (access->isWrite) {
        writeAccesses.emplace_back(access);
    } else {
        readAccesses.emplace_back(access);
    }
};

//=============================
// getAccesses
//=============================
void ISLInterval::getAccesses(std::vector<const MemAccess*>& out, bool onlyWrite) const
{
    // we want to return raw pointers, so we need to map unique_ptr to raw pointer
    std::transform(
        writeAccesses.begin(),
        writeAccesses.end(),
        std::back_inserter(out),
        [](const auto& w) { return w.get(); });

    if (!onlyWrite) {
        std::transform(
            readAccesses.begin(),
            readAccesses.end(),
            std::back_inserter(out),
            [](const auto& r) { return r.get(); });
    }
}

//=============================
// getConflicts
//=============================
void ISLInterval::getConflicts(
    std::vector<const MemAccess*>& out,
    const MemAccess& access,
    bool onlyWrite) const
{
    for (const auto& w : writeAccesses)
        if (w->conflicts(access))
            out.push_back(w.get());

    if (!onlyWrite)
        for (const auto& r : readAccesses)
            if (r->conflicts(access))
                out.push_back(r.get());
}

// non const return
void ISLInterval::getConflicts(
    std::vector<MemAccess*>& out,
    const MemAccess& access,
    bool onlyWrite) const
{
    for (auto& w : writeAccesses)
        if (w->conflicts(access))
            out.push_back(w.get());

    if (!onlyWrite)
        for (auto& r : readAccesses)
            if (r->conflicts(access))
                out.push_back(r.get());
}

} /* end namespace ISL */