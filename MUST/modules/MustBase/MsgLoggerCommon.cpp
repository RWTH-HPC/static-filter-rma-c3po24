/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file MsgLoggerCommon.cpp
 *       @see MUST::MsgLoggerCommon.
 *
 *  @date 04.01.2023
 *  @author Sebastian Grabowski
 */

#include "MsgLoggerCommon.hpp"

#include <sstream>

namespace must
{

#ifdef ENABLE_STACKTRACE

auto format_stacktrace(std::list<MustStackLevelInfo> const& stacktrace, char const* newline)
    -> std::string
{
    std::stringstream out;
    int depth = 0;
    for (auto stackIter = stacktrace.begin(); stackIter != stacktrace.end(); stackIter++, depth++) {
        if (depth != 0) {
            out << newline;
        }
        out << "#" << depth << "  " << stackIter->symName << "@" << stackIter->fileModule << ":"
            << stackIter->lineOffset;
    }
    return out.str();
}

#endif

MsgLoggerBase::MsgLoggerBase() : myPIdModule(NULL), myLIdModule(NULL)
{
    myErrorCodeFile = getenv("MUST_RETURNCODE_FILE");
}

bool MsgLoggerBase::emitErrorCodeToFile()
{
    if (!myErrorCodeFile)
        return false;

    std::ofstream fp;
    fp.open(myErrorCodeFile, std::ofstream::out | std::ofstream::app);
    fp << std::to_string(myErrorCode) << std::endl;
    fp.close();
    return true;
}

MsgLoggerBase::~MsgLoggerBase() { emitErrorCodeToFile(); }

void MsgLoggerBase::rememberErrorcode(int msgType)
{
    if (msgType > myErrorCode) {
        myErrorCode = msgType;
        emitErrorCodeToFile();
    }
}

} // namespace must
