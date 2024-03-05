/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_InitTSanSyncClock.h
 *       @see I_InitTSanSyncClock.
 *
 *  @date 22.10.22
 *  @author Simon Schwitanski
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"
#include "I_ChannelId.h"
#include "MustTypes.h"

#ifndef I_INITTSANSYNCCLOCK_H
#define I_INITTSANSYNCCLOCK_H

typedef unsigned long uptr;

/**
 * Stores the TSan sync clock of the current thread in the given storage.
 */
class I_InitTSanSyncClock : public gti::I_Module
{
  public:
    /**
     * Sets the given storage to the TSan sync clock of the current thread.
     * @param pStorage pointer to the storage in which the TSan sync clock should be stored
     * @return @see GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN init(void** pStorage) = 0;

}; /*class I_InitTSanSyncClock*/

#endif /*I_INITTSANSYNCCLOCK_H*/
