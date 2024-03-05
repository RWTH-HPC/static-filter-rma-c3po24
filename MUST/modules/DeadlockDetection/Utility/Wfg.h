/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file Wfg.h
 *       @see must::Wfg.
 *
 *  @date 12.08.2011
 *  @author Tobias Hilbrich, Mathias Korepkat, Joachim Protze
 */

#include <map>
#include <list>

#include "MustEnums.h"

#ifndef WFG_H
#define WFG_H

namespace must
{
/**
 * Information on a node.
 */
class NodeInfo
{
  public:
    NodeInfo(void);
    ArcType type;
    std::list<int> inArcs;
    int outCount;
};

/**
 * Instance of a deadlock detector.
 * Currently this is an adapter to the WFG library from Umpire.
 *
 * It is used for a one shot deadlock detection. Add the arcs of the
 * WFG first and issue "detectDeadlock" afterwards.
 */
class Wfg
{
  public:
    /**
     * Constructor.
     */
    Wfg(void);

    /**
     * Destructor.
     */
    virtual ~Wfg(void);

    /**
     * Adds an arc of the given type from the node "from" to the node "to".
     * Each node must only use one arc type.
     *
     * @param from node.
     * @param to node.
     * @param type of arc.
     * @return true iff successful.
     */
    bool addArc(int from, int to, ArcType type);

    /**
     * Starts deadlock detection.
     *
     * @param outHasDeadlock is set to true if there is a deadlock in the WFG, set to false
     * otherwise.
     * @param outDeadlockedNodes set to list with core nodes of the deadlock if a deadlock is
     * detected.
     * @return true iff successful.
     */
    bool detectDeadlock(bool* outHasDeadlock, std::list<int>* outDeadlockedNodes);

  protected:
    std::map<int, NodeInfo> myWfg;

    int myNextInternalId;
    std::map<int, int> myNodeIdToInternalId;
    std::map<int, int> myInternalIdToNodeId;

}; /*Wfg*/
} /*namespace must*/

#endif /*WFG_H*/
