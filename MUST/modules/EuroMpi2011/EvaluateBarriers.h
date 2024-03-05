/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file EvaluateBarriers.h
 *       @see MUST::EvaluateBarriers.
 *
 *  @date 06.05.2011
 *  @author Tobias Hilbrich
 */

#include "ModuleBase.h"
#include "I_EvaluateBarriers.h"

#include <string>

#ifndef EVALUATEBARRIERS_H
#define EVALUATEBARRIERS_H

using namespace gti;

namespace must
{
/**
 * Implementation of I_EvaluateBarriers.
 */
class EvaluateBarriers : public gti::ModuleBase<EvaluateBarriers, I_EvaluateBarriers>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    EvaluateBarriers(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~EvaluateBarriers(void);

    /**
     * @see I_Template::newBarrier
     */
    GTI_ANALYSIS_RETURN newBarrier(unsigned long long tMin, unsigned long long tMax);

  protected:
    unsigned long long mySumDilation;
    unsigned long long myNumBarriers;
    long long myMinDilation;
    long long myMaxDilation;
};
} // namespace must

#endif /*EVALUATEBARRIERS_H*/
