/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TransferStatistics.h
 *       @see MUST::TransferStatistics.
 *
 *  @date 16.09.2011
 *  @author Tobias Hilbrich, Mathias Korepkat, Joachim Protze
 */

#include "ModuleBase.h"
#include "I_ParallelIdAnalysis.h"
#include "I_ArgumentAnalysis.h"
#include "I_CollectiveCondition.h"
#include "I_CommTrack.h"
#include "I_DatatypeTrack.h"
#include "TransferReductionApi.h"

#include "I_TransferStatistics.h"

#include <sys/time.h>

#ifndef TRANSFERSTATISTICS_H
#define TRANSFERSTATISTICS_H

using namespace gti;

namespace must
{
/**
 * Template for correctness checks interface implementation.
 */
class TransferStatistics : public gti::ModuleBase<TransferStatistics, I_TransferStatistics>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    TransferStatistics(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~TransferStatistics(void);

    /**
     * @see I_TransferStatistics::init
     */
    gti::GTI_ANALYSIS_RETURN init(MustParallelId pId);

    /**
     * @see I_TransferStatistics::sendP2P
     */
    gti::GTI_ANALYSIS_RETURN sendP2P(MustParallelId pId, MustDatatypeType type, int count);

    /**
     * @see I_TransferStatistics::collNoTransfer
     */
    gti::GTI_ANALYSIS_RETURN collNoTransfer(MustParallelId pId, MustCommType comm);

    /**
     * @see I_TransferStatistics::collSend
     */
    gti::GTI_ANALYSIS_RETURN
    collSend(MustParallelId pId, MustCommType comm, MustDatatypeType type, int count);

    /**
     * @see I_TransferStatistics::collSendN
     */
    gti::GTI_ANALYSIS_RETURN
    collSendN(MustParallelId pId, MustCommType comm, MustDatatypeType type, int count);

    /**
     * @see I_TransferStatistics::collSendCounts
     */
    gti::GTI_ANALYSIS_RETURN
    collSendCounts(MustParallelId pId, MustCommType comm, MustDatatypeType type, int* counts);

    /**
     * @see I_TransferStatistics::collSendTypes
     */
    gti::GTI_ANALYSIS_RETURN
    collSendTypes(MustParallelId pId, MustCommType comm, MustDatatypeType* types, int* counts);

    /**
     * @see I_TransferStatistics::collRecvBalance
     */
    gti::GTI_ANALYSIS_RETURN
    collRecvBalance(MustParallelId pId, MustCommType comm, int coll, int root);

  protected:
    I_ParallelIdAnalysis* myPIdMod;
    I_CommTrack* myCommTrack;
    I_DatatypeTrack* myTypeTrack;
    I_CollectiveCondition* myCollCond; /*Not used, but we have a dependency ...*/

    uint64_t myP2PSent;
    uint64_t myNumP2POps;
    uint64_t myCollSent;
    uint64_t myNumCollOps;
    struct timeval myIntervalStart;
    int myWorldRank;
    int myWorldSize;

    statisticsIntervalP myFP; /**< Function pointer to create an interval statistics.*/

    bool checkAndCompleteIntervall(MustParallelId pId, MustCommType comm);
};
} // namespace must

#endif /*TRANSFERSTATISTICS_H*/
