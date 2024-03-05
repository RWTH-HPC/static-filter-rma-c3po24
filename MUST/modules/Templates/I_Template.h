/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_Template.h
 *       @see I_Template.
 *
 *  @date 21.01.2011
 *  @author Tobias Hilbrich
 */

#include "I_Module.h"
#include "GtiEnums.h" //TODO Needs to be renamed to GTI enums

#ifndef I_TEMPLATE_H
#define I_TEMPLATE_H

/**
 * Template interface for an analysis.
 */
class I_Template : public gti::I_Module
{
  public:
    /**
     *
     */
    virtual gti::GTI_ANALYSIS_RETURN analysisFunction(void) = 0;

}; /*class I_Template*/

#endif /*I_TEMPLATE_H*/
