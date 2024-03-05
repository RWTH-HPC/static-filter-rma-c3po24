/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file ISLNode.cpp
 *
 *  @date 19.06.2023
 *  @author Sem Klauke
 */
#include "ISLInterval.h"
#include "ISLNode.h"
#include "MemAccess.h"
#include <cstdint>
#include <memory>
#include <utility>

namespace ISL
{

//=============================
// Destructor.
//=============================
ISLNode::~ISLNode() = default;

//=============================
// Copy constructor.
//=============================
ISLNode::ISLNode(ISLNode const& n) = default;

//=============================
// Constructors.
//=============================
ISLNode::ISLNode() = default;

ISLNode::ISLNode(MemAddress key) : key(key) {}

ISLNode::ISLNode(MemAddress key, int lvl) : key(key)
{
    /* idea: set capacity for this node so we don't need as much resizes.
       but: this segfaults / overwrites stack somehow */
    // forward.reserve(lvl+1);
    // markers.reserve(lvl+1);
}


void ISLNode::setForward(int lvl, const std::shared_ptr<ISLNode>& node)
{
    if (lvl >= forward.size()) {
        forward.emplace_back(node);
    } else {
        forward[lvl] = std::shared_ptr<ISLNode>(node);
    }
}

int ISLNode::getLevel() const { return static_cast<int>(forward.size()); }

} /* end namespace ISL */