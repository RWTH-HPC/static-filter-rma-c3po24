/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file AppThrAnn.h
 *       @see must::AppThrAnn.
 *
 *  @date 27.06.2017
 *  @author Simon Schwitanski
 */

#include "ModuleBase.h"
#include "I_ParallelIdAnalysis.h"
#include "I_BaseConstants.h"
#include "I_WinTrack.h"
#include "I_TSan.h"
#include "I_AppThrAnn.h"
#include "OneSidedChecksApi.h"
#include "DataPool.h"
#include <set>

#ifndef APPTHRANN_H
#define APPTHRANN_H

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

class AppThrAnn : public gti::ModuleBase<AppThrAnn, I_AppThrAnn>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    AppThrAnn(const char* instanceName);

    /**
     * @see I_AppThrAnn:winLock
     */
    GTI_ANALYSIS_RETURN
    winLock(MustParallelId pId, MustLocationId lId, int lock_type, int rank, MustWinType win);

    /**
     * @see I_AppThrAnn:winUnlock
     */
    GTI_ANALYSIS_RETURN
    winUnlock(MustParallelId pId, MustLocationId lId, int rank, MustWinType win);

    GTI_ANALYSIS_RETURN annotateHappensBefore(void* ann);

    /**
     * Destructor.
     */
    virtual ~AppThrAnn(void);

  protected:
    I_ParallelIdAnalysis* myPIdMod;
    I_BaseConstants* myConsts;
    I_WinTrack* myWinMod;
    I_TSan* myTSanMod;
    propagateWinLockP myPropagateWinLock;
    propagateWinUnlockP myPropagateWinUnlock;

    int translateRank(I_Comm* comm, int rank);
    std::map<MustWinType, AnnData*> myRWLockMap;

}; /*class AppThrAnn*/

} /*namespace must*/

#endif /*APPTHRANN_H*/
