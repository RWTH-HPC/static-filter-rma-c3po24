/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_GenerateLocationId.h
 * Provides unique location IDs.
 *
 * This functionality is separated from the InitLocationId module and shared
 * between application and tool thread.
 *
 *  @date 09.02.2023
 *  @author Simon Schwitanski
 */

#include "GtiEnums.h"
#include "I_Module.h"

#include "BaseIds.h"

#ifndef I_GENERATELOCATIONID_H
#define I_GENERATELOCATIONID_H

class I_GenerateLocationId : public gti::I_Module
{
  public:
    /**
     * Provides a unique location ID.
     * @return location ID
     */
    virtual MustLocationId getNextLocationId() = 0;

}; /*class I_GenerateLocationId*/

#endif /*I_GENERATELOCATIONID_H*/
