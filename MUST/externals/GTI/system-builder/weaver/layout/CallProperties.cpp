/* This file is part of GTI (Generic Tool Infrastructure)
 *
 * Copyright (C)
 *  2008-2019 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2013-2019 RWTH Aachen University, Federal Republic of Germany
 *
 * See the LICENSE file in the package base directory for details
 */

/**
 * @file CallProperties.cpp
 * 		@see gti::weaver::CallProperties
 *
 * @author Tobias Hilbrich
 * @date 05.08.2010
 */

#include "CallProperties.h"

using namespace gti::weaver::layout;

long long CallProperties::ourNextUniqueId = 0;

//=============================
// CallProperties
//=============================
CallProperties::CallProperties(void) {}

//=============================
// CallProperties
//=============================
CallProperties::CallProperties(Call* call, bool isOnApplication)
    : myCall(call), myIsOnApplication(isOnApplication)
{
}

//=============================
// ~CallProperties
//=============================
CallProperties::~CallProperties(void)
{
    // Memory of the arguments and the calls is
    // managed by their repsective singletons
    myUsedArgs.clear();
    myArgsToReceive.clear();
    myOpsToExecute.clear();
    myCall = NULL;
}

//=============================
// getUsedArgs
//=============================
InputList CallProperties::getUsedArgs(void) { return myUsedArgs; }

//=============================
// getArgsToReceive
//=============================
InputSet CallProperties::getArgsToReceive(void) { return myArgsToReceive; }

//=============================
// getInRecordUniqueId
//=============================
long long CallProperties::getInRecordUniqueId(void) { return myInRecordUniqueId; }

//=============================
// setInRecordUniqueId
//=============================
void CallProperties::setInRecordUniqueId(long long uid) { myInRecordUniqueId = uid; }

//=============================
// needsWrapper
//=============================
bool CallProperties::needsWrapper(void) const
{
    /**
     * Important:
     * It is necessary to use the arguments to receive here!
     * Even if this level does not uses the arguments of the
     * call (i.e. myUsedArgs is empty), wrapping the call
     * will generate information that is consumed by other
     * levels if myArgsToReceive is not empty. Thus, the
     * information needs to be collected for forwarding in
     * that case.
     */
    if (myInformationRequired) {
        if (myIsOnApplication || myCall->isWrappedEverywhere() || myCall->isWrappedDown() ||
            (myCall->isWrappedAcross() && myWrapAcrossIsCreatedOnLevel))
            return true;
    } else if (myCall->isFinalizer()) {
        return true;
    }

    return false;
}

//=============================
// needsReceival
//=============================
bool CallProperties::needsReceival(void) const
{
    if (myIsOnApplication)
        return false;

    if (myInformationRequired || myCall->isFinalizer())
        return true;

    return false;
}

//=============================
// getNextUniqueId
//=============================
long long CallProperties::getNextUniqueId(void)
{
    long long temp = ourNextUniqueId;
    ourNextUniqueId++;
    return temp;
}

//=============================
// receiveListEqual
//=============================
bool CallProperties::receiveListEqual(const InputSet& other)
{
    //	InputSet::iterator i,j;

    if (other.size() != myArgsToReceive.size())
        return false;

    for (auto i = other.begin(), j = myArgsToReceive.begin();
         i != other.end() && j != myArgsToReceive.end();
         i++, j++) {
        if ((*i)->getName() != (*j)->getName())
            return false;
    }
    return true;
}

//=============================
// addOperationToExecute
//=============================
void CallProperties::addOperationToExecute(Operation* op, int id)
{
    // check whether this operation and id is already listed
    std::list<std::pair<Operation*, int>>::iterator i;
    for (i = myOpsToExecute.begin(); i != myOpsToExecute.end(); i++) {
        if (i->first == op && i->second == id)
            return;
    }

    // add to list of operations
    myOpsToExecute.push_back(std::make_pair(op, id));
}

//=============================
// print
//=============================
std::ostream& CallProperties::print(std::ostream& out) const
{

    if (!myCall)
        return out;

    out << "call=" << myCall->getName() << ", "
        << "in-uid=" << myInRecordUniqueId << ", "
        << "on-app=" << myIsOnApplication << ", "
        << "needs-receival=" << needsReceival() << ", "
        << "needs-wrapper=" << needsWrapper() << std::endl;

    out << "used-args={";

    for (auto i = myUsedArgs.begin(); i != myUsedArgs.end(); i++) {
        if (i != myUsedArgs.begin())
            out << ", ";
        out << (*i)->getName();
    }

    out << "}" << std::endl << "to-receive-args={";

    for (auto i = myArgsToReceive.begin(); i != myArgsToReceive.end(); i++) {
        if (i != myArgsToReceive.begin())
            out << ", ";
        out << (*i)->getName();
    }

    out << "}" << std::endl << "ops-to-execute={";

    std::list<std::pair<Operation*, int>>::const_iterator opIter;
    for (opIter = myOpsToExecute.begin(); opIter != myOpsToExecute.end(); opIter++) {
        if (opIter != myOpsToExecute.begin())
            out << ", ";
        out << opIter->first->getName() << ":" << opIter->second;
    }

    out << "}" << std::endl;

    return out;
}

//=============================
// getMappedOperations
//=============================
std::list<std::pair<Operation*, int>> CallProperties::getMappedOperations(void)
{
    return myOpsToExecute;
}

//=============================
// setWrapAcrossCallAsCreatedOnLevel
//=============================
void CallProperties::setWrapAcrossCallAsCreatedOnLevel(void)
{
    myWrapAcrossIsCreatedOnLevel = true;
}

/*EOF*/
