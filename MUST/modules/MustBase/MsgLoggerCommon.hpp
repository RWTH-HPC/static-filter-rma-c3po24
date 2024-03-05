/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file MsgLoggerCommon.hpp
 *       @see MUST::MsgLoggerCommon.
 *
 *  @date 04.01.2023
 *  @author Sebastian Grabowski
 */

#ifndef MUST_MSGLOGGERCOMMON_HPP
#define MUST_MSGLOGGERCOMMON_HPP

#include <list>
#include <string>
#include <fstream>

#include "I_FinishNotify.h"
#include "I_CreateMessage.h"
#include "I_ParallelIdAnalysis.h"
#include "I_LocationAnalysis.h"
#include "LocationInfo.h"

#include "BaseIds.h"
#include "MustOutputdir.h"

namespace must
{

#ifdef ENABLE_STACKTRACE
auto format_stacktrace(std::list<MustStackLevelInfo> const& stacktrace, char const* newline = "\n")
    -> std::string;
#endif

class MsgLoggerBase
{
  public:
    MsgLoggerBase();
    ~MsgLoggerBase();
    void rememberErrorcode(int msgType);
    bool emitErrorCodeToFile();
    int myErrorCode{0};
    bool myGotFinish{false};
    const char* myErrorCodeFile{nullptr};

  protected:
    I_ParallelIdAnalysis* myPIdModule;
    I_LocationAnalysis* myLIdModule;
};

} // namespace must

#endif // MUST_MSGLOGGERCOMMON_HPP
