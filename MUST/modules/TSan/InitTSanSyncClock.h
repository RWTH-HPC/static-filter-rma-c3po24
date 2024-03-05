/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file InitTSanSyncClock.h
 *       @see InitTSanSyncClock.
 *
 *  @date 22.10.22
 *  @author Simon Schwitanski
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"
#include "I_ChannelId.h"
#include "MustTypes.h"
#include "ModuleBase.h"
#include "I_InitTSanSyncClock.h"
#include "I_TSan.h"
#include "DataPool.h"

#ifndef INITTSANSYNCCLOCK_H
#define INITTSANSYNCCLOCK_H

typedef unsigned long uptr;

using namespace gti;

namespace must
{
struct AnnData;
__thread DataPool<AnnData, 4>* myAnnPool;

// Struct for TSan annotations.
// Note: We do not store anything in this struct, it is rather a
// dummy struct with overriden new and delete operators for the
// DataPool handling. We use the object's address for the TSan
// annotations.
struct AnnData {
    // overload new/delete to use DataPool for memory management.
    void* operator new(size_t size) { return myAnnPool->getData(); }
    void operator delete(void* p, size_t) { retData<AnnData, 4>(p); }
};

/**
 * Implementation of I_TSanSyncClock.
 */
class InitTSanSyncClock : public gti::ModuleBase<InitTSanSyncClock, I_InitTSanSyncClock>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    InitTSanSyncClock(const char* instanceName);
    ~InitTSanSyncClock();

    GTI_ANALYSIS_RETURN init(void** pStorage);

  private:
    I_TSan* myTSanMod;

}; /*class InitTSanSyncClock*/

} // namespace must

#endif /*INITTSANSYNCCLOCK_H*/