/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_DatatypePredefs.h
 *       @see DatatypePredefs.
 *
 *  @date 18.02.2011
 *  @author Tobias Hilbrich
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"

#ifndef I_DATATYPEPREDEFS_H
#define I_DATATYPEPREDEFS_H

/**
 * Interface for basic datatype handling helper.
 * It is used to trigger I_DatatypeTrack::addPredefineds.
 * It should be invoked after (post) MPI_Init, and
 * will take care to call I_DatatypeTrack::addPredefineds.
 * DatatypeTrack should depend on this module to guarantee
 * that this is possible.
 *
 * The main motivation for this module is the need to
 * query the handles for several of the Fortran types from
 * within Fortran, so this can't be done as an operation,
 * as an operation can't inject Fortran code.
 */
class I_DatatypePredefs : public gti::I_Module
{
  public:
    /**
     * Analysis function used to gather datatype handles and
     * call I_DatatypeTrack::addPredefineds.
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    virtual gti::GTI_ANALYSIS_RETURN propagate(MustParallelId pId) = 0;

}; /*class I_DatatypePredefs*/

#endif /*I_DATATYPEPREDEFS_H*/
