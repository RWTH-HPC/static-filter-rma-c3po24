/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef TSANINTERCEPTOR_H
#define TSANINTERCEPTOR_H

/**
 * @file TSanInterceptor.h
 *       @see MUST::TSanInterceptor.
 *
 *  @date 26.07.2023
 *  @author Felix Tomski
 *  @author Sem Klauke
 */

#include "BaseApi.h"
#include "I_CreateMessage.h"
#include "I_GenerateLocationId.h"
#include "I_ParallelIdAnalysis.h"
#include "I_InitLocationId.h"
#include "I_InitParallelId.h"
#include "I_TSanInterceptor.h"
#include "TSanInterceptorApi.h"
#include "ModuleBase.h"
#include <map>
#include <set>

namespace must
{

/* In one local VC cycle we want to store which acceses happend to a specific mem address.
   The mem address is the key in a map. */
struct AccessesForAddress {
    // 0: write 1: read pc
    std::set<void*> pc[2];
    MustAddressType endAddr[2]{0, 0};
    AccessesForAddress(int8_t isRead, MustAddressType addr_, void* pc_)
    {
        pc[isRead].emplace(pc_);
        endAddr[isRead] = addr_;
    };
};

/* Temporary holds arrays/vector to be propergated.*/
struct BulkAccesses {
    /* Store start and end memory address.
     * With n accesses: addr.size() == endAddr.size() == n */
    std::vector<void*> addr;
    std::vector<void*> endAddr;
    /* Multiple program counters per interval.
     * So we also need to store number of pc for each interval. */
    std::vector<void*> pc;
    std::vector<uint64_t> pcNum; // pcNum.size() == n
    BulkAccesses(size_t reservation)
    {
        addr.reserve(reservation);
        pc.reserve(reservation);
        endAddr.reserve(reservation);
        pcNum.reserve(reservation);
    }
};

class TSanInterceptor : public gti::ModuleBase<TSanInterceptor, I_TSanInterceptor>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    explicit TSanInterceptor(const char* instanceName);

    /**
     * Destructor.
     */
    ~TSanInterceptor() override;

    gti::GTI_ANALYSIS_RETURN init() override;
    gti::GTI_ANALYSIS_RETURN fini() override;
    gti::GTI_ANALYSIS_RETURN access(void* pc, int8_t isRead, void* addr, int64_t count);
    gti::GTI_ANALYSIS_RETURN tick() override;

    MustParallelId getParallelId() const;
    MustLocationId getLocationId(const void* pc) const;

  private:
    I_CreateMessage* myLogger;
    I_GenerateLocationId* myGenLId;
    I_ParallelIdAnalysis* myPIdMod;
    I_InitParallelId* myParallelInit;
    I_InitLocationId* myLocationInit;
    handleNewLocationP myNewLocFunc;
    propagateTSanAccessP myPropagateAccess;
    propagateTSanAccessBulkP myPropagateAccessBulk;

    // store accesses in one VC cycle
    std::map<void*, AccessesForAddress> currentAccesses;
};
} // namespace must

#endif /*TSANINTERCEPTOR_H*/