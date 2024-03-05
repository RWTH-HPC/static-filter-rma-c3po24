/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file MsgMatchReduction.h
 *       @see MUST::MsgMatchReduction.
 *
 *  @date 17.03.2011
 *  @author Tobias Hilbrich, Mathias Korepkat
 */

#include "ModuleBase.h"
#include "I_ParallelIdAnalysis.h"
#include "I_P2PMatch.h"
#include "I_RequestTrack.h"

#include "I_MsgMatchReduction.h"

#include <string>

#ifndef MSGMATCHREDUCTION_H
#define MSGMATCHREDUCTION_H

using namespace gti;

namespace must
{
/**
 * Template for correctness checks interface implementation.
 */
class MsgMatchReduction : public gti::ModuleBase<MsgMatchReduction, I_MsgMatchReduction>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    MsgMatchReduction(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~MsgMatchReduction(void);

    /**
     * @see I_MsgMatchReduction::send
     */
    GTI_ANALYSIS_RETURN send(
        MustParallelId pId,
        MustLocationId lId,
        int dest,
        int tag,
        MustCommType comm,
        MustDatatypeType type,
        int count,
        int mode,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels);

    /**
     * @see I_MsgMatchReduction::isend
     */
    GTI_ANALYSIS_RETURN isend(
        MustParallelId pId,
        MustLocationId lId,
        int dest,
        int tag,
        MustCommType comm,
        MustDatatypeType type,
        int count,
        int mode,
        MustRequestType request,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels);

    /**
     * @see I_MsgMatchReduction::recv
     */
    GTI_ANALYSIS_RETURN recv(
        MustParallelId pId,
        MustLocationId lId,
        int source,
        int tag,
        MustCommType comm,
        MustDatatypeType type,
        int count,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels);

    /**
     * @see I_MsgMatchReduction::irecv
     */
    GTI_ANALYSIS_RETURN irecv(
        MustParallelId pId,
        MustLocationId lId,
        int source,
        int tag,
        MustCommType comm,
        MustDatatypeType type,
        int count,
        MustRequestType request,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels);

    /**
     * @see I_MsgMatchReduction::startPersistent
     */
    GTI_ANALYSIS_RETURN startPersistent(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels);

    /**
     * @see I_Reduction::timeout.
     */
    void timeout(void);

  protected:
    I_ParallelIdAnalysis* myPIdMod;
    I_P2PMatch* myLM;
    I_RequestTrack* myRT;
};
} // namespace must

#endif /*MSGMATCHREDUCTION_H*/
