/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_CollectUnmapped.h
 *       @see I_CollectUnmapped.
 *
 *  @date 07.07.2022
 *  @author Felix Tomski
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"
#include "I_DatatypeTrack.h"

#ifndef I_COLLECTUNMAPPED_H
#define I_COLLECTUNMAPPED_H

class I_CollectUnmapped : public gti::I_Module
{
  public:
    /**
     * Collect an MPI function to which no proper
     * analysis is mapped.
     *
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN collectFunction(MustParallelId pId, MustLocationId lId) = 0;

    /**
     * Print out the collected unmapped function names.
     *
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN
    printUnmappedFunctions(MustParallelId pId, MustLocationId lId) = 0;

}; /*class I_CollectUnmapped*/

#endif /*I_COLLECTUNMAPPED_H*/
