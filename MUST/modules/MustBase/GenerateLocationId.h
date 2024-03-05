/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file GenerateLocationId.h
 *       @see must::GenerateLocationId.
 *
 *  @date 09.02.2023
 *  @author Simon Schwitanski
 */

#include "BaseApi.h"
#include "I_GenerateLocationId.h"
#include "LocationInfo.h"
#include "ModuleBase.h"
#include "mustConfig.h"

#ifndef GENERATELOCATIONID_H
#define GENERATELOCATIONID_H

using namespace gti;

namespace must
{

class GenerateLocationId : public gti::ModuleBase<GenerateLocationId, I_GenerateLocationId, false>
{
  private:
    std::atomic<MustLocationId> nextLocationId;

  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    GenerateLocationId(const char* instanceName);

    /**
     * @see I_GenerateLocationId::getNextLocationId
     */
    MustLocationId getNextLocationId();

}; /*class GenerateLocationId */
} // namespace must

#endif /*GENERATELOCATIONID_H*/
