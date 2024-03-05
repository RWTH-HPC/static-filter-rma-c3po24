/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file CompletionCondition.h
 *       @see CompletionCondition.
 *
 *  @date 16.12.2011
 *  @author Tobias Hilbrich, Mathias Korepkat, Fabian Haensel, Joachim Protze
 */

#include "ModuleBase.h"
#include "I_RequestTrack.h"

#include "CompletionConditionApi.h"
#include "I_CompletionCondition.h"

#include <string>

#ifndef COMPLETIONCONDITION_H
#define COMPLETIONCONDITION_H

using namespace gti;

namespace must
{
/**
 * Template for correctness checks interface implementation.
 */
class CompletionCondition : public gti::ModuleBase<CompletionCondition, I_CompletionCondition>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    CompletionCondition(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~CompletionCondition(void);

    /**
     * @see I_CompletionCondition::wait
     */
    GTI_ANALYSIS_RETURN wait(MustParallelId pId, MustLocationId lId, MustRequestType request);

    /**
     * @see I_CompletionCondition::waitAny
     */
    GTI_ANALYSIS_RETURN
    waitAny(MustParallelId pId, MustLocationId lId, MustRequestType* requests, int count);

    /**
     * @see I_CompletionCondition::waitAll
     */
    GTI_ANALYSIS_RETURN
    waitAll(MustParallelId pId, MustLocationId lId, MustRequestType* requests, int count);

    /**
     * @see I_CompletionCondition::waitSome
     */
    GTI_ANALYSIS_RETURN
    waitSome(MustParallelId pId, MustLocationId lId, MustRequestType* requests, int count);

  protected:
    I_RequestTrack* myRTrack;
    MustRequestType* myTempArray;
    int myTempArraySize;

    propagateReducedWaitP myPReducedWait;
    propagateReducedWaitallP myPReducedWaitall;
    propagateReducedWaitsomeP myPReducedWaitsome;
    propagateReducedWaitanyP myPReducedWaitany;

    /**
     * Resizes temporary array if necessary.
     */
    void checkTempSize(int count);

    /**
     * Filles all active requests of the given array into the temporary
     * array. Returns the size of the temporary array.
     * @return number of active requests that are stored in the
     *              temporary request array.
     */
    int
    fillTempArray(int count, MustRequestType* requests, MustParallelId pId, int* outNumProcNull);
};
} // namespace must

#endif /* COMPLETIONCONDITION_H */
