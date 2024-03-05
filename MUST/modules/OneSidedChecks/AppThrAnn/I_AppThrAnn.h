/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_AppThrAnn.h
 *       @see I_AppThrAnn
 *
 *  @date 27.06.2017
 *  @author Simon Schwitanski
 */

#include "GtiEnums.h"

#include "MustEnums.h"
#include "BaseIds.h"
#include "MustTypes.h"

#ifndef I_APPTHRANN_H
#define I_APPTHRANN_H

/**
 * Annotates happens-before and happens-after relations at RMA calls
 * and passes the calls to other interested modules (e.g. RMATrack).
 *
 * Dependencies (in listed order):
 * - ParallelIdAnalysis
 * - BaseConstants
 * - WinTrack
 * - TSan
 */
class I_AppThrAnn : public gti::I_Module
{
  public:
    virtual gti::GTI_ANALYSIS_RETURN
    winLock(MustParallelId pId, MustLocationId lId, int lock_type, int rank, MustWinType win) = 0;

    virtual gti::GTI_ANALYSIS_RETURN
    winUnlock(MustParallelId pId, MustLocationId lId, int rank, MustWinType win) = 0;

    virtual gti::GTI_ANALYSIS_RETURN annotateHappensBefore(void* ann) = 0;

}; /*class I_AppThrAnn*/

#endif /*I_APPTHRANN_H*/
