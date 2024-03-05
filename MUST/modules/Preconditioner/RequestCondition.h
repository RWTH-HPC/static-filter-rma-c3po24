/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file RequestCondition.h
 *       @see MUST::RequestCondition.
 *
 *  @date 06.06.2011
 *  @author Joachim Protze
 */

#include "ModuleBase.h"

#include "I_RequestCondition.h"
#include "safe_ptr.h"

#include <string>

#ifndef REQUESTCONDITION_H
#define REQUESTCONDITION_H

using namespace gti;

namespace must
{
/**
 * Template for correctness checks interface implementation.
 */
class RequestCondition : public gti::ModuleBase<RequestCondition, I_RequestCondition>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    RequestCondition(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~RequestCondition(void);

    /**
     * @see I_RequestCondition::addPredefineds
     */
    GTI_ANALYSIS_RETURN addPredefineds(int anySource);

    /**
     * @see I_RequestCondition::complete
     */
    GTI_ANALYSIS_RETURN complete(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType request,
        int flag,
        int statusSource);

    /**
     * @see I_RequestCondition::completeAny
     */
    GTI_ANALYSIS_RETURN completeAny(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType* requests,
        int count,
        int index,
        int flag,
        int statusSource);

    /**
     * @see I_RequestCondition::completeArray
     */
    GTI_ANALYSIS_RETURN completeArray(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType* requests,
        int count,
        int flag,
        int* statusSources);

    /**
     * @see I_RequestCondition::completeSome
     */
    GTI_ANALYSIS_RETURN completeSome(
        MustParallelId pId,
        MustLocationId lId,
        MustRequestType* requests,
        int count,
        int* indices,
        int numIndices,
        int* statusSources);

    /**
     * @see I_RequestCondition::commIdup
     */
    GTI_ANALYSIS_RETURN commIdup(
        MustParallelId pId,
        MustLocationId lId,
        MustCommType comm,
        void* newcomm,
        MustRequestType requests);

  protected:
    int myAnySource;

    typedef std::map<MustRequestType, std::function<int()>> CallbackMap;
    sf::contfree_safe_ptr<CallbackMap> myCallbacks;
    executeCommDupP myPCommDup;

    propagateRequestRealCompleteP myPComplete;
    propagateRequestsRealCompleteP myPCompletes;
};
} // namespace must

#endif /* REQUESTCONDITION_H */
