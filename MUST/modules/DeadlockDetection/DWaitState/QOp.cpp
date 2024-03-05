/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file QOp.cpp
 *       @see must::QOp.
 *
 *  @date 28.02.2013
 *  @author Tobias Hilbrich
 */

#include "QOp.h"
#include "DWaitState.h"

#include <assert.h>

#include <sstream>
#include <fstream>

using namespace must;

//=============================
// QOp
//=============================
QOp::QOp(DWaitState* dws, MustParallelId pId, MustLocationId lId, MustLTimeStamp ts)
    : myState(dws), myPId(pId), myLId(lId), myTS(ts), myRank(-1), myRefCount(1)
{
    assert(dws);
    myRank = dws->getParallelIdAnalysis()->getInfoForId(pId).rank;
}

//=============================
// QOp
//=============================
QOp::~QOp() {}

void QOp::checkAlive(void) { assert(!deleted); }

//=============================
// getIssuerRank
//=============================
int QOp::getIssuerRank()
{
    checkAlive();
    return myRank;
}

//=============================
// getPId
//=============================
MustParallelId QOp::getPId(void)
{
    checkAlive();
    return myPId;
}

//=============================
// getLId
//=============================
MustLocationId QOp::getLId(void)
{
    checkAlive();
    return myLId;
}

//=============================
// getTimeStamp
//=============================
MustLTimeStamp QOp::getTimeStamp(void)
{
    checkAlive();
    return myTS;
}

//=============================
// incRefCount
//=============================
int QOp::incRefCount(void)
{
    checkAlive();
    myRefCount++;
    return myRefCount;
}

//=============================
// erase
//=============================
int QOp::erase()
{
    checkAlive();
    myRefCount--;
    int temp = myRefCount;

    if (myRefCount == 0) {
        deleted = true;
        delete (this);
    }

    return temp;
}

//=============================
// printAsDot
//=============================
std::string QOp::printAsDot(std::ofstream& out, std::string nodePrefix, std::string color)
{
    checkAlive();
    std::string nodeName = nodePrefix + "_op";
    out << nodeName << "[label=\"{" << printVariablesAsLabelString()
        << "}\", shape=Mrecord, fillcolor=" << color << ", style=filled"
        << "];" << std::endl;

    return nodeName;
}

//=============================
// printVariablesAsLabelString
//=============================
std::string QOp::printVariablesAsLabelString(void)
{
    checkAlive();
    std::stringstream stream;
    stream << myState->getLocationlIdAnalysis()->getInfoForId(myPId, myLId).callName
           << "|refCount=" << myRefCount;
    return stream.str();
}

//=============================
// asOpCommunicationP2P
//=============================
QOpCommunicationP2P* QOp::asOpCommunicationP2P(void)
{
    checkAlive();
    return NULL;
}

//=============================
// asOpCommunicationColl
//=============================
QOpCommunicationColl* QOp::asOpCommunicationColl(void)
{
    checkAlive();
    return NULL;
}

//=============================
// getPingPongNodes
//=============================
std::set<int> QOp::getPingPongNodes(void)
{
    checkAlive();
    std::set<int> ret;
    return ret;
}

/*EOF*/
