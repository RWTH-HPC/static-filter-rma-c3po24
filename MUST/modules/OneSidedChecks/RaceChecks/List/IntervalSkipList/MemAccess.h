/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file MemAccess.h
 *
 *  @date 19.06.2023
 *  @author Sem Klauke
 */
#ifndef MemAccess_h
#define MemAccess_h

#include "Clock.h"
#include "BaseIds.h"
#include <memory>
#include "MustTypes.h"

namespace ISL
{

using MemAddress = MustAddressType;

class MemAccess
{
  public:
    bool isWrite;                 //
    bool isAtomic;                // meaning RMA atomic, not compatible with cpp atomics
    MustRMAId rmaId;              // id of this rma op (is = 0 for non-RMA access)
    MustParallelId pId;           //
    MustLocationId lId;           // = 0 when not set (get lazy with pc)
    Clock vectorClock1;           // start of concurrent region (only VC if non-RMA access)
    Clock vectorClock2;           // end of concurrent region (not set if non-RMA access)
    MemAddress left{};            // lowest memaddress of this access
    MemAddress right{};           // highest memaddress of this access
    MustDatatypeType datatype{0}; // Datatype of RMA call (= 0 for local)
    MemAddress datatypeSize{4};   // Size of `datatype` in bytes
    void* pc{nullptr};            // Save pc of non-RMA accesses for lazy lId generation

    ~MemAccess();
    MemAccess(MemAccess const& m);
    MemAccess();

    MemAccess(
        bool isWrite,
        bool isAtomic,
        MustRMAId rmaId,
        MustParallelId pId,
        MustLocationId lId,
        MemAddress left,
        MemAddress right,
        MustDatatypeType dt,
        MemAddress size,
        void* pc);

    /**
     * Add start and end VC of concurrent region of RMA access
     */
    void insertVectorClock(Clock vc1, Clock vc2);

    /**
     * Add VC of non-RMA access
     *
     * @overload
     */
    void insertVectorClock(Clock vc);

    /**
     * Check if this access conflicts / has an data race with other access
     *
     * @param access other access to
     * @return true if conflicts with other access
     */
    bool conflicts(const MemAccess& access) const;

    /**
     * @return true if this is RMA access with concurrent region
     */
    bool hasConcurrentRegion() const;

    /**
     * Factory for origin RMA access
     */
    static MemAccess* createOriginAccess(
        bool isWrite,
        MustRMAId rmaId,
        MustParallelId pId,
        MustLocationId lId,
        MemAddress left,
        MemAddress right,
        MustDatatypeType dt,
        MemAddress size);

    /**
     * Factory for target RMA access
     */
    static MemAccess* createTargetAccess(
        bool isWrite,
        bool isAtomic,
        MustRMAId rmaId,
        MustParallelId pId,
        MustLocationId lId,
        MemAddress left,
        MemAddress right,
        MustDatatypeType dt,
        MemAddress size);

    /**
     * Factory for local non-RMA access
     */
    static MemAccess* createLocalAccess(
        bool isWrite,
        MustParallelId pId,
        MustLocationId lId,
        MemAddress left,
        MemAddress right,
        void* pc);
};

} /* end namespace ISL */

#endif