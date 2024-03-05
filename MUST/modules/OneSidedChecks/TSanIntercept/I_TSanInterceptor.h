/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_TSanInterceptor.h
 *       @see I_TSanInterceptor.
 *
 *  This module is used to intercept TSan instrumentation calls.
 *  TSan uses compiler-instrumentation to add TSan function calls
 *  to LOAD/STORE instructions. Those TSan function calls can be
 *  intercepted at runtime to do a custom LOAD/STORE tracking.
 *  This module is used by the ISL race checks.
 *
 *  @date 26.07.2023
 *  @author Felix Tomski
 */

#include "I_Module.h"
#include "GtiEnums.h"
#include "BaseIds.h"

#include <string>

#ifndef I_TSANINTERCEPTOR_H
#define I_TSANINTERCEPTOR_H

class I_TSanInterceptor : public gti::I_Module
{
  public:
    virtual gti::GTI_ANALYSIS_RETURN fini() = 0;
    virtual gti::GTI_ANALYSIS_RETURN init() = 0;
    virtual gti::GTI_ANALYSIS_RETURN tick() = 0;
}; /*class I_TSanInterceptor*/

#endif /*I_TSANINTERCEPTOR_H*/