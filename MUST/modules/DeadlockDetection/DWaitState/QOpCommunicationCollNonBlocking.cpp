/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file QOpCommunicationCollNonBlocking.cpp
 *       @see must::QOpCommunicationCollNonBlocking.
 *
 *  @date 28.07.2015
 *  @author Tobias Hilbrich
 */

#include "QOpCommunicationCollNonBlocking.h"
#include "DWaitState.h"

using namespace must;

//=============================
// QOpCommunicationCollNonBlocking
//=============================
QOpCommunicationCollNonBlocking::QOpCommunicationCollNonBlocking(
    DWaitState* dws,
    MustParallelId pId,
    MustLocationId lId,
    MustLTimeStamp ts,
    I_CommPersistent* comm,
    MustCollCommType collType,
    MustLTimeStamp waveNumberInComm,
    MustRequestType request)
    : QOpCommunicationColl(dws, pId, lId, ts, comm, collType, waveNumberInComm), myRequest(request)
{
    // Nothing to do
}

//=============================
// ~QOpCommunicationCollNonBlockingQOpCommunicationCollNonBlocking
//=============================
QOpCommunicationCollNonBlocking::~QOpCommunicationCollNonBlocking(void)
{
    // Nothing to do
}

//=============================
// hasRequest
//=============================
bool QOpCommunicationCollNonBlocking::hasRequest(void)
{
    checkAlive();
    return true;
}

//=============================
// getRequest
//=============================
MustRequestType QOpCommunicationCollNonBlocking::getRequest(void)
{
    checkAlive();
    return myRequest;
}

//=============================
// printVariablesAsLabelString
//=============================
std::string QOpCommunicationCollNonBlocking::printVariablesAsLabelString(void)
{
    checkAlive();
    std::stringstream stream;
    stream << "|request=" << myRequest;

    return QOpCommunicationColl::printVariablesAsLabelString() + stream.str();
}

//=============================
// blocks
//=============================
bool QOpCommunicationCollNonBlocking::blocks(void)
{
    checkAlive();
    // Non-blocking collective doesn't blocks!
    return false;
}

/*EOF*/
