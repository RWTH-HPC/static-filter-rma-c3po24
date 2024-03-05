/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_DP2PListener.h
 *       @see I_DP2PListener.
 *
 *  @date 01.03.2013
 *  @author Tobias Hilbrich
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"

#ifndef I_DP2PLISTENER_H
#define I_DP2PLISTENER_H

namespace must
{
/**
 * Interface for listener on P2P events.
 */
class I_DP2PListener
{
  public:
    /**
     * Notifies the listener of a new P2P operation that at DP2PMatch.
     * (Only of ranks that send to this TBON node, not of remote sends)
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @see QOpCommunicationP2PNonBlocking::QOpCommunicationP2PNonBlocking
     * @param outIsActive pointer to storage for bool value, listener sets this to true if
     *               he considers this op as "active" and to false otherwise.
     * @return the logical timestamp that was given to this op.
     */
    virtual MustLTimeStamp newP2POp(
        MustParallelId pId,
        MustLocationId lId,
        I_CommPersistent* comm,
        bool isSend,
        int sourceTarget,
        bool isWc,
        MustSendMode mode,
        int tag,
        bool hasRequest,
        MustRequestType request,
        bool* outIsActive) = 0;

    /**
     * Notifies the listener of a match for a P2P receive call.
     *
     * @param pIdRecv parallel ID of the receive.
     * @param recvTS logical timestamp of the receive.
     * @param pIdSend parallel ID of the matching send.
     * @param sendTS logical timestamp of the matching send.
     */
    virtual void notifyP2PRecvMatch(
        MustParallelId pIdRecv,
        MustLTimeStamp recvTS,
        MustParallelId pIdSend,
        MustLTimeStamp sendTS) = 0;

}; /*class I_DP2PListener*/
} // namespace must

#endif /*I_DP2PLISTENER_H*/
