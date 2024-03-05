/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file ISLNode.h
 *
 *  @date 19.06.2023
 *  @author Sem Klauke
 */
#ifndef ISLNode_h
#define ISLNode_h

#include "MemAccess.h"
#include "ISLInterval.h"
#include <cstdint>
#include <memory>
#include <vector>
#include <set>

namespace ISL
{

class ISLNode
{
  public:
    MemAddress key{};

    /* see paper DOI:10.1007/BFb0028258 */
    std::vector<std::shared_ptr<ISLNode>> forward; // forward pointer
    std::vector<std::set<std::shared_ptr<ISLInterval>>> markers; // intervals going out of this node
    std::set<std::shared_ptr<ISLInterval>> owners; // intervals containing `key` of this node

    ~ISLNode();
    ISLNode(ISLNode const& n);
    ISLNode();

    /**
     * Constructor
     */
    ISLNode(MemAddress key);

    /**
     * Constructor
     *
     * @param key search key
     * @param lvl height of this node 
     */
    ISLNode(MemAddress key, int lvl);

    /**
     * setter for forward pionter at lvl
     *
     * @param lvl from 0 to this->getLevel()-1
     * @param node shared_ptr of node to point to at lvl (will increase ref count)
     */
    void setForward(int lvl, const std::shared_ptr<ISLNode>& node);

    /**
     * getter for height of this node
     * 
     * @return num. of levels / forward pointers of this node
     */
    int getLevel() const;
};

} /* end namespace ISL */

#endif