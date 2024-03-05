/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_EvaluateBarriers.h
 *       @see I_EvaluateBarriers
 *
 *  @date 06.05.2011
 *  @author Tobias Hilbrich
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"

#ifndef I_EVALUATEBARRIERS_H
#define I_EVALUATEBARRIERS_H

/**
 * Evaluates reduced or unreduced barriers.
 *
 * Dependencies (order as listed):
 * X
 */
class I_EvaluateBarriers : public gti::I_Module
{
  public:
    /**
     * Notifies the module of a new barrier.
     *
     * @param tMin minimal execution time of barrier.
     * @param tMax maxium execution time of barrier.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    newBarrier(unsigned long long tMin, unsigned long long tMax) = 0;
}; /*class I_EvaluateBarriers*/

#endif /*I_EVALUATEBARRIERS_H*/
