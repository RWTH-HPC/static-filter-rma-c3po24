/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file FinishNotify.h
 *       @see MUST::FinishNotify.
 *
 *  @date 05.08.2013
 *  @author Tobias Hilbrich
 */

#include "ModuleBase.h"
#include "I_ParallelIdAnalysis.h"
#include "I_ArgumentAnalysis.h"
#include "I_CreateMessage.h"

#include "I_FinishNotify.h"

#include <list>

#ifndef FINISHNOTIFY_H
#define FINISHNOTIFY_H

using namespace gti;

namespace must
{
/**
 * I_FinishNotify implementation.
 */
class FinishNotify : public gti::ModuleBase<FinishNotify, I_FinishNotify>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    FinishNotify(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~FinishNotify(void);

    /**
     * @see I_FinishNotify::finish
     */
    GTI_ANALYSIS_RETURN finish(void);

    /**
     * @see I_FinishNotify::addListener
     */
    void addListener(I_FinishListener* listener);

  protected:
    std::list<I_FinishListener*> myListeners;
};
} // namespace must

#endif /*FINISHNOTIFY_H*/
