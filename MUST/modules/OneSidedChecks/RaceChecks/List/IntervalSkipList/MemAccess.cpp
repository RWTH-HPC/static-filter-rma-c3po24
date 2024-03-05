/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file MemAccess.cpp
 *
 *  @date 19.06.2023
 *  @author Sem Klauke
 */
#include "MemAccess.h"
#include <memory>
#include <utility>

namespace ISL
{

//=============================
// Destructor.
//=============================
MemAccess::~MemAccess() = default;

//=============================
// Copy constructor.
//=============================
MemAccess::MemAccess(MemAccess const& m) = default;

//=============================
// Constructor.
//=============================
MemAccess::MemAccess() = default;
MemAccess::MemAccess(
    bool isWrite,
    bool isAtomic,
    MustRMAId rmaId,
    MustParallelId pId,
    MustLocationId lId,
    MemAddress left,
    MemAddress right,
    MustDatatypeType dt,
    MemAddress size,
    void* pc)
    : isWrite{isWrite}, isAtomic{isAtomic}, rmaId{rmaId}, pId{pId}, lId{lId}, left{left},
      right{right}, datatype{dt}, datatypeSize{size}, pc{pc}
{
}

//=============================
// insertVectorClock (for non-RMA access)
//=============================
void MemAccess::insertVectorClock(Clock vc)
{
    this->rmaId = 0;
    this->vectorClock1 = vc;
}

//=============================
// insertVectorClock (for RMA access with concurrent region)
//=============================
void MemAccess::insertVectorClock(Clock vc1, Clock vc2)
{
    this->vectorClock1 = vc1;
    this->vectorClock2 = vc2;
}

//=============================
// conflicts
//=============================
bool MemAccess::conflicts(const MemAccess& access) const
{

    // don't conflict with yourself
    if (this == &access)
        return false;

    // if both are read -> no conflict
    if (!this->isWrite and !access.isWrite)
        return false;

    // checking vector clocks
    if (!this->hasConcurrentRegion() and !access.hasConcurrentRegion())
        return false; // both are local

    // VC(local) < VC1(RMA) || VC(local) >= VC2(RMA) && VC(local) != VC2(RMA)
    if (!this->hasConcurrentRegion() and access.hasConcurrentRegion()) {
        // this is local, access has concurrent region
        if ((this->vectorClock1 < access.vectorClock1) or
            (access.vectorClock2 <= this->vectorClock1) and
                not(this->vectorClock1 == access.vectorClock1))
            return false; // not inside concurrent region
        else
            return true;
    }

    if (this->hasConcurrentRegion() and !access.hasConcurrentRegion()) {
        // this has concurrent region, access is local
        if ((access.vectorClock1 < this->vectorClock1) or
            (this->vectorClock2 <= access.vectorClock1) and
                not(this->vectorClock1 == access.vectorClock1))
            return false; // not inside concurrent region
        else
            return true;
    }

    // both have concurrent regions
    if ((access.vectorClock2 <= this->vectorClock1) !=
            (this->vectorClock2 <= access.vectorClock1) and
        not(access.vectorClock2 == this->vectorClock2))
        return false; // concurrent regions don't overlap (!= meaning xor here)

    // check atomic
    if (this->isAtomic and access.isAtomic) {
        // check displacement // TODO
        if (this->datatype == access.datatype and
            (this->left - access.left) % this->datatypeSize == 0)
            return false;
    }

    // there must be a conflict
    return true;
};

//=============================
// hasConcurrentRegion
//=============================
bool MemAccess::hasConcurrentRegion() const { return this->rmaId != 0; }


//=============================
// createOriginAccess
//=============================
MemAccess* MemAccess::createOriginAccess(
    bool isWrite,
    MustRMAId rmaId,
    MustParallelId pId,
    MustLocationId lId,
    MemAddress left,
    MemAddress right,
    MustDatatypeType dt,
    MemAddress size)
{
    return new MemAccess(isWrite, false, rmaId, pId, lId, left, right, dt, size, nullptr);
};

//=============================
// createTargetAccess
//=============================
MemAccess* MemAccess::createTargetAccess(
    bool isWrite,
    bool isAtomic,
    MustRMAId rmaId,
    MustParallelId pId,
    MustLocationId lId,
    MemAddress left,
    MemAddress right,
    MustDatatypeType dt,
    MemAddress size)
{
    return new MemAccess(isWrite, isAtomic, rmaId, pId, lId, left, right, dt, size, nullptr);
};

//=============================
// createLocalAccess
//=============================
MemAccess* MemAccess::createLocalAccess(
    bool isWrite,
    MustParallelId pId,
    MustLocationId lId,
    MemAddress left,
    MemAddress right,
    void* pc)
{
    return new MemAccess(isWrite, false, 0, pId, lId, left, right, 0, 0, pc);
};

} /* end namespace ISL */