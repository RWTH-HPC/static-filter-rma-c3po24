/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file CollectUnmapped.h
 *       @see MUST::CollectUnmapped.
 *
 *  @date 07.07.2022
 *  @author Felix Tomski
 */

#include "ModuleBase.h"
#include "I_ParallelIdAnalysis.h"
#include "I_LocationAnalysis.h"
#include "I_CreateMessage.h"

#include "I_CollectUnmapped.h"

#include <string>
#include <set>
#include <list>

#ifndef COLLECTUNMAPPED_H
#define COLLECTUNMAPPED_H

using namespace gti;

namespace must
{
/**
 * CollectUnmapped for correctness checks interface implementation.
 */
class CollectUnmapped : public gti::ModuleBase<CollectUnmapped, I_CollectUnmapped>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    CollectUnmapped(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~CollectUnmapped(void);

    /**
     * @see I_CollectUnmapped::collectFunction.
     */
    GTI_ANALYSIS_RETURN collectFunction(MustParallelId pId, MustLocationId lId);

    /**
     * @see I_CollectUnmapped::printUnmappedFunctions.
     */
    GTI_ANALYSIS_RETURN printUnmappedFunctions(MustParallelId pId, MustLocationId lId);

  protected:
    I_ParallelIdAnalysis* myPIdMod;
    I_CreateMessage* myLogger;
    I_LocationAnalysis* myLIdMod;

    std::map<std::string, std::pair<MustParallelId, MustLocationId>> myUnmappedFunctions;
};
} // namespace must

#endif /*COLLECTUNMAPPED_H*/
