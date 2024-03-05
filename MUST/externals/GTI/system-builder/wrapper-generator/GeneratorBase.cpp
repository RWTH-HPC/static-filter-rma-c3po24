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
 * @file GeneratorBase.cpp
 *		@see gti::codegen::GeneratorBase
 *
 * @author Tobias Hilbrich
 * @date 13.08.2010
 */

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <assert.h>

#include <dlfcn.h>

#include "Verbose.h"

#include "GeneratorBase.h"

using namespace gti::codegen;

//=============================
// GeneratorBase
//=============================
GeneratorBase::GeneratorBase()
    : myOutDir(), mySourceName(""), myHeaderName(""), myLogName(""), myRecordHeaders(),
      mySourceOut(), myHeaderOut(), myLogOut(), myCommMods(), myIntraCommMods(), myDownCommMods(),
      myAnalyses(), myAnalysisMods(), myImpl(NULL), myDocument(NULL)
{
    myProfiling = false;
    if (getenv("GTI_PROFILE") != NULL) {
        if (atoi(getenv("GTI_PROFILE")) == 1)
            myProfiling = true;
    }
}

//=============================
// ~GeneratorBase
//=============================
GeneratorBase::~GeneratorBase(void)
{
    if (myImpl)
        freeRecordGenerationImplementation();

    if (myDocument)
        xmlFreeDoc(myDocument);

    std::list<ModuleInfo*>::iterator iter;
    for (iter = myAnalysisMods.begin(); iter != myAnalysisMods.end(); iter++) {
        if (*iter)
            delete (*iter);
    }
    myAnalysisMods.clear();
    myAnalyses.clear();

    mySourceOut.close();
    myHeaderOut.close();
    myLogOut.close();
}

//=============================
// openInput
//=============================
SpecificationNode GeneratorBase::openInput(std::string inputFile)
{
    SpecificationNode currentPointer;

    myDocument = xmlParseFile(inputFile.c_str());

    if (myDocument == NULL) {
        std::cerr << "Error loading input XML (" << inputFile << ")"
                  << "(" << __FILE__ << ":" << __LINE__ << ")" << std::endl;
        return NULL;
    }

    currentPointer = xmlDocGetRootElement(myDocument);

    if (currentPointer == NULL ||
        (xmlStrcmp(currentPointer()->name, (const xmlChar*)myGetRootNodeName().c_str()) != 0)) {
        std::cerr << "Error: Document does not contains the root node (\"" << myGetRootNodeName()
                  << "\")"
                  << "(" << __FILE__ << ":" << __LINE__ << ")" << std::endl;
        if (currentPointer)
            std::cerr << "Found \"" << currentPointer()->name << "\" instead!" << std::endl;
        xmlFreeDoc(myDocument);
        return NULL;
    }

    return currentPointer;
}

//=============================
// readSettings
//=============================
bool GeneratorBase::readSettings(SpecificationNode node)
{
    SpecificationNode child;

    //==Child: output-dir
    child = node.findChildNodeNamedOrErr(
        "output-dir",
        "|  |-->Error: the settings node has no \"output-dir\" child.");
    if (!child)
        return false;

    myOutDir = child.getNodeContent();

    //==Child: source-filename-out
    child = node.findChildNodeNamedOrErr(
        "source-filename-out",
        "|  |-->Error: the settings node has no \"source-filename-out\" child.");
    if (!child)
        return false;

    mySourceName = child.getNodeContent();

    //==Child: header-filename-out
    child = node.findChildNodeNamedOrErr(
        "header-filename-out",
        "|  |-->Error: the settings node has no \"header-filename-out\" child.");
    if (!child)
        return false;

    myHeaderName = child.getNodeContent();

    //==Child: log-filename-out
    child = node.findChildNodeNamedOrErr(
        "log-filename-out",
        "|  |-->Error: the settings node has no \"log-filename-out\" child.");
    if (!child)
        return false;

    myLogName = child.getNodeContent();

    //==Open the output files
    mySourceOut.open((myOutDir + "/" + mySourceName).c_str());
    if (mySourceOut.fail()) {
        std::cerr << "|  |-->Error: could not open output source file: "
                  << myOutDir + "/" + mySourceName << std::endl;
        return false;
    }

    myHeaderOut.open((myOutDir + "/" + myHeaderName).c_str());
    if (myHeaderOut.fail()) {
        std::cerr << "|  |-->Error: could not open output header file: "
                  << myOutDir + "/" + myHeaderName << std::endl;
        return false;
    }

    myLogOut.open((myOutDir + "/" + myLogName).c_str());
    if (myLogOut.fail()) {
        std::cerr << "|  |-->Error: could not open output log file: " << myOutDir + "/" + myLogName
                  << std::endl;
        return false;
    }

    //==Write initial comments to source and header
    mySourceOut << "/**" << std::endl
                << " * @file " << mySourceName << std::endl
                << " *     Generated source file." << std::endl
                << " *" << std::endl
                << " * Generated by the GTI  " << myGetGeneratorName() << "!" << std::endl
                << " * Do not edit this file, changes will likely be non-permanent!" << std::endl
                << " */" << std::endl
                << std::endl
                << "#include \"" << myHeaderName << "\"" << std::endl
                << std::endl;

    myHeaderOut << "/**" << std::endl
                << " * @file " << myHeaderName << std::endl
                << " *     Generated header file." << std::endl
                << " *" << std::endl
                << " * Generated by the GTI " << myGetGeneratorName() << "!" << std::endl
                << " * Do not edit this file, changes will likely be non-permanent!" << std::endl
                << " */" << std::endl
                << std::endl;

    // Set the class Name
    //=================================
    myClassName = mySourceName;
    size_t pos;
    do {
        pos = myClassName.find_first_of(".-&^*#`´\"§$%/\\()=?");

        if (pos == std::string::npos)
            break;

        myClassName[pos] = '_';
    } while (pos != std::string::npos);

    return true;
}

//=============================
// getRecordGenerationImplementation
//=============================
bool GeneratorBase::getRecordGenerationImplementation(gti::I_RecordGenerator** ppImpl)
{
    /*
     * @todo we need a final solution here, right now we use dlopen/dlsym calls.
     *             In the long term I would prefer to use pnmpi instead, but that always
     *             requires MPI at the moment, thus we would need to call the generators
     *             with "mpirun -np 1 generator.exe" which is not so lucky.
     */

    if (getenv("GTI_RECORD_GEN_IMPL") == NULL) {
        std::cerr << "ERROR: you need to specify the environmental GTI_RECORD_GEN_IMPL which "
                     "should contain a filepath for the record generation library to use."
                  << std::endl;
        return false;
    }

    void* libHandle = 0;
    I_RecordGenerator* (*getImplP)(void);
    libHandle = dlopen(getenv("GTI_RECORD_GEN_IMPL"), RTLD_LAZY);

    if (!libHandle) {
        std::cerr
            << "ERROR: could not find the shared library specified with GTI_RECORD_GEN_IMPL (\""
            << getenv("GTI_RECORD_GEN_IMPL")
            << "\"), check this specification, it must be a filepath to the record generation "
               "implementation."
            << std::endl
            << "dl error message: " << dlerror() << std::endl;
        return false;
    }

    getImplP = (I_RecordGenerator * (*)(void)) dlsym(libHandle, "getImplementation");

    if (!getImplP) {
        std::cerr << "ERROR: the library specified with GTI_RECORD_GEN_IMPL (\""
                  << getenv("GTI_RECORD_GEN_IMPL")
                  << "\"), has no \"getImplementation\" function, check the library implementation."
                  << std::endl
                  << "dl error message: " << dlerror() << std::endl;
        return false;
    }

    if (ppImpl)
        *ppImpl = getImplP();

    /*
        int argc = 1;
        char **argv = new char*[1];
        argv[0] = new char[32];
        sprintf (argv[0], "wrappgen");
        MPI_Init (&argc,&argv);
        PNMPI_Service_descriptor_t service;
        PNMPI_modHandle_t handle;
        int err;

        err = PNMPI_Service_GetModuleByName("record_gen_implementation", &handle);
        if (err != PNMPI_SUCCESS)
        {
                std::cerr
                        << "Error: Failed to get implementation for record generator!"
                        << std::endl;
                return false;
        }

        err = PNMPI_Service_GetServiceByName(handle, "getInstance", "pp", &service);
        if (err != PNMPI_SUCCESS)
        {
                std::cerr
                        << "Error: Module for record generation provides no \"getInstance\"
    service."
                        << std::endl;
                return false;
        }

        ((getInstance_t) service.fct) (ppImpl, "");

    MPI_Finalize();
    */
    return true;
}

//=============================
// freeRecordGenerationImplementation
//=============================
bool GeneratorBase::freeRecordGenerationImplementation(void)
{
    delete myImpl;

    // TODO needs to be refined
    /*PNMPI_Service_descriptor_t service;
    PNMPI_modHandle_t handle;
    int err;

    err = PNMPI_Service_GetModuleByName("record_gen_implementation", &handle);

    err = PNMPI_Service_GetServiceByName(handle, "freeInstance", "p", &service);
    assert (err == PNMPI_SUCCESS);
    ((freeInstance_t) service.fct) (myImpl);
    myImpl = NULL;
    */

    return true;
}

//=============================
// readHeaders
//=============================
bool GeneratorBase::readAndPrintHeaders(SpecificationNode node)
{
    SpecificationNode child;

    // childs: header
    child = node.findChildNodeNamed("header");

    mySourceOut << "/***** HEADERS *****/" << std::endl;

    while (child) {
        std::string headerName = child.getNodeContent();
        bool isSystem = false;
        std::string isSystemString;

        if (child.getAttribute("is-system", &isSystemString)) {
            if (isSystemString == "yes")
                isSystem = true;

            if (isSystemString != "yes" && isSystemString != "no") {
                std::cerr << "|  |-->Error: an is-system attribute od a header node specifies the "
                             "value \""
                          << isSystemString
                          << "\", which is an invalid value; valid are \"yes\" and \"no\"."
                          << std::endl;
                return false;
            }
        }

        if (isSystem)
            myHeaderOut << "#include <" << headerName << ">" << std::endl;
        else
            myHeaderOut << "#include \"" << headerName << "\"" << std::endl;

        // next
        child = child.findSiblingNamed("header");
    }

    mySourceOut << std::endl;

    return true;
}

//=============================
// readCommunications
//=============================
bool GeneratorBase::readCommunications(SpecificationNode node)
{
    SpecificationNode child;

    //==Childs: communication
    child = node.findChildNodeNamed("communication");

    while (child) {
        std::string modName = child.getNodeContent();
        std::string idString;
        std::string isIntraString, isDownString;
        bool isIntra = false, isDown = false;

        if (!child.getAttributeOrErr(
                "id",
                "|  |-->Error: a communication node has no \"id\" attribute.",
                &idString))
            return false;

        if (child.getAttribute("is-intra", &isIntraString)) {
            if (isIntraString != "yes" && isIntraString != "no") {
                std::cerr << "ERROR: invalid specification for \"is-intra\" in a communication "
                             "node, valid are \"yes\" and \"no\", the specification was \""
                          << isIntraString << "\"." << std::endl;
                return false;
            }

            if (isIntraString == "yes")
                isIntra = true;
        }

        if (child.getAttribute("is-down", &isDownString)) {
            if (isDownString != "yes" && isDownString != "no") {
                std::cerr << "ERROR: invalid specification for \"is-down\" in a communication "
                             "node, valid are \"yes\" and \"no\", the specification was \""
                          << isDownString << "\"." << std::endl;
                return false;
            }

            if (isDownString == "yes")
                isDown = true;
        }

        if (isIntra) {
            myIntraCommMods.insert(std::make_pair(atoi(idString.c_str()), modName));
        } else if (isDown) {
            myDownCommMods.insert(std::make_pair(atoi(idString.c_str()), modName));
        } else {
            myCommMods.insert(std::make_pair(atoi(idString.c_str()), modName));
        }

        // next
        child = child.findSiblingNamed("communication");
    }

    return true;
}

//=============================
// readAnalyses
//=============================
bool GeneratorBase::readAnalyses(SpecificationNode node)
{
    SpecificationNode child, subChild;

    //==Childs: analysis
    child = node.findChildNodeNamed("analysis");

    while (child) {
        std::string sid;
        ModuleInfo info;
        AnalysisFunction function;
        std::string reductionStr;
        std::string timeoutStr;
        bool isReduction = false;
        bool listensToTimeouts = false;
        std::string channelIdStr;
        bool needsChannelId = false;
        std::string continuousStr;
        bool continuous = false;

        // subchild: analysis-id
        subChild = child.findChildNodeNamedOrErr(
            "analysis-id",
            "|  |-->Error: an analysis node has no \"analysis-id\" child.");
        if (!subChild)
            return false;
        sid = subChild.getNodeContent();

        // Attribute: reduction
        if (child.getAttribute("reduction", &reductionStr)) {
            if (reductionStr != "no" && reductionStr != "yes") {
                std::cerr << "Error: the attribute \"reduction\" of an analysis node has an "
                             "unknown value of \""
                          << reductionStr << "\"; valid are \"yes\" and \"no\"." << std::endl;
                return false;
            }

            if (reductionStr == "yes")
                isReduction = true;
        }
        info.isReduction = isReduction;

        // Attribute: listensToTimeouts
        if (child.getAttribute("listens-to-timeouts", &timeoutStr)) {
            if (timeoutStr != "no" && timeoutStr != "yes") {
                std::cerr << "Error: the attribute \"listens-to-timeouts\" of an analysis node has "
                             "an unknown value of \""
                          << timeoutStr << "\"; valid are \"yes\" and \"no\"." << std::endl;
                return false;
            }

            if (timeoutStr == "yes")
                listensToTimeouts = true;
        }
        info.listensToTimeouts = listensToTimeouts;

        // Attribute: needs-channel-id
        if (child.getAttribute("needs-channel-id", &channelIdStr)) {
            if (channelIdStr != "no" && channelIdStr != "yes") {
                std::cerr << "Error: the attribute \"needs-channel-id\" of an analysis node has an "
                             "unknown value of \""
                          << channelIdStr << "\"; valid are \"yes\" and \"no\"." << std::endl;
                return false;
            }

            if (channelIdStr == "yes")
                needsChannelId = true;
        }
        function.needsChannelId = needsChannelId;

        // Attribute: needs-channel-id
        if (child.getAttribute("continuous", &continuousStr)) {
            if (continuousStr != "no" && continuousStr != "yes") {
                std::cerr << "Error: the attribute \"continuous\" of an analysis node has an "
                             "unknown value of \""
                          << continuousStr << "\"; valid are \"yes\" and \"no\"." << std::endl;
                return false;
            }

            if (continuousStr == "yes")
                continuous = true;
        }
        info.continuous = continuous;

        // subchild: analysis-name
        subChild = child.findChildNodeNamedOrErr(
            "analysis-name",
            "|  |-->Error: an analysis node has no \"analysis-name\" child.");
        if (!subChild)
            return false;
        info.name = subChild.getNodeContent();

        // subchild: analysis-datatype
        subChild = child.findChildNodeNamedOrErr(
            "analysis-datatype",
            "|  |-->Error: an analysis node has no \"analysis-datatype\" child.");
        if (!subChild)
            return false;
        info.datatype = subChild.getNodeContent();

        // subchild: analysis-function
        subChild = child.findChildNodeNamedOrErr(
            "analysis-function",
            "|  |-->Error: an analysis node has no \"analysis-function\" child.");
        if (!subChild)
            return false;
        function.function = subChild.getNodeContent();

        // Is there already a module wiht this name and type ?
        std::list<ModuleInfo*>::iterator iter;
        for (iter = myAnalysisMods.begin(); iter != myAnalysisMods.end(); iter++) {
            if ((*iter)->name == info.name && (*iter)->datatype == info.datatype) {
                break;
            }
        }

        if (iter != myAnalysisMods.end()) {
            // Use the existing ModuleInfo
            function.info = *iter;
            myAnalyses.insert(std::make_pair(sid, function));
        } else {
            info.index = myAnalysisMods.size();

            // New ModuleInfo
            ModuleInfo* newInfo = new ModuleInfo;
            *newInfo = info;

            myAnalysisMods.push_back(newInfo);

            function.info = newInfo;
            myAnalyses.insert(std::make_pair(sid, function));
        }

        // next
        child = child.findSiblingNamed("analysis");
    }

    return true;
}

//=============================
// readRecord
//=============================
bool GeneratorBase::readRecord(
    SpecificationNode node,
    int* pOutUid,
    gti::I_RecordType** pOutRecord,
    std::list<std::string>* args,
    std::list<std::string>* arrayArgs)
{
    SpecificationNode child, subchild;
    int uid;
    std::string uidString;

    std::string toFind = "element";

    gti::I_RecordDescription* pDesc;
    myImpl->createRecordDescription(&pDesc);

    std::list<std::string>::iterator argIter;

    if (!pOutRecord || !args || !arrayArgs)
        return false;
    args->clear();
    arrayArgs->clear();

    //==Attribute: uid
    if (!node.getAttributeOrErr(
            "uid",
            "|  |-->Error: a record node has no \"uid\" attribute.",
            &uidString))
        return false;
    uid = atoi(uidString.c_str());
    if (pOutUid)
        *pOutUid = uid;

    //==Childs: (element|array-element)*
    child = node.findChildNodeNamed(toFind);
    if (!child) {
        toFind = "array-element";
        child = node.findChildNodeNamed(toFind);
    }

    while (child) {
        std::string name, type, fromCall, asArg, lenArg;

        //==Child: name
        subchild = child.findChildNodeNamedOrErr(
            "name",
            "|  |-->Error: a " + toFind + " node has no \"name\" child.");
        if (!subchild)
            return false;
        name = subchild.getNodeContent();

        //==Child: type
        subchild = child.findChildNodeNamedOrErr(
            "type",
            "|  |-->Error: a " + toFind + " node has no \"type\" child.");
        if (!subchild)
            return false;
        type = subchild.getNodeContent();

        //==Child: from-call
        subchild = child.findChildNodeNamedOrErr(
            "from-call",
            "|  |-->Error: a " + toFind + " node has no \"from-call\" child.");
        if (!subchild)
            return false;
        fromCall = subchild.getNodeContent();

        //==Child: as-arg
        subchild = child.findChildNodeNamedOrErr(
            "as-arg",
            "|  |-->Error: a " + toFind + " node has no \"as-arg\" child.");
        if (!subchild)
            return false;
        asArg = subchild.getNodeContent();

        //==Add the argument to the record description
        if (toFind == "element") {
            pDesc->addArgument(name, type, fromCall, asArg);
            args->push_back(name);
        } else {
            //==Child: length-argument
            subchild = child.findChildNodeNamedOrErr(
                "length-argument",
                "|  |-->Error: a " + toFind + " node has no \"length-argument\" child.");
            if (!subchild)
                return false;
            lenArg = subchild.getNodeContent();

            //==Check whether given len arg already is in record ...
            for (argIter = args->begin(); argIter != args->end(); argIter++) {
                if (*argIter == lenArg)
                    break;
            }

            if (argIter == args->end()) {
                // not present->add it now
                pDesc->addArgument(lenArg, "int", fromCall, lenArg);
                args->push_back(lenArg);
            }

            //==create
            size_t pos = type.find_first_of("*");
            if (pos != std::string::npos)
                type.replace(pos, 1, "");
            pDesc->addArrayArgument(name, type, fromCall, asArg, lenArg);

            arrayArgs->push_back(name);
        }

        // next
        child = child.findSiblingNamed(toFind);
        if (!child && toFind == "element") {
            toFind = "array-element";
            child = node.findChildNodeNamed(toFind);
        }
    }

    //==Create record
    if (pDesc->createRecord(uid, pOutRecord) != GTI_SUCCESS) {
        std::cerr << "|  |-->Internal Error: failed to create record." << std::endl;
        return false;
    }

    //==Delete the record description
    pDesc->deleteObject();

    return true;
}

//=============================
// printRecord
//=============================
bool GeneratorBase::printRecord(
    SpecificationNode node,
    int* pOutUid,
    std::string* pOutFreeCode,
    std::ostream& out,
    gti::I_RecordType* pExistingRecord,
    std::list<std::string> existingArgs,
    std::list<std::string> existingArrayArgs)
{
    gti::I_RecordType* pRecord;

    std::list<std::string> args, arrayArgs;
    std::list<std::string>::iterator argIter;
    std::string uidString;
    char temp[64];

    if (!pOutUid)
        return false;

    //==Read in the record and create it
    if (!readRecord(node, pOutUid, &pRecord, &args, &arrayArgs))
        return false;

    sprintf(temp, "%d", *pOutUid);
    uidString = temp;

    //==Print record creation
    std::string recordName = std::string("record") + uidString,
                serBufName = std::string("buf") + uidString,
                serLenName = std::string("buf") + uidString + "Len";
    std::string defCode, initCode, freeCode, serCode, tempCode;

    pRecord->createInstance(recordName, &defCode);
    pRecord->initInstance(recordName, &initCode);
    if (pOutFreeCode)
        pRecord->freeInstance(recordName, pOutFreeCode);

    out << "        //Create Record: " << *pOutUid << std::endl
        << "        " + defCode << ";" << std::endl
        << "        " + initCode << std::endl;

    // set value for args (must come first)
    for (argIter = args.begin(); argIter != args.end(); argIter++) {
        std::string from = *argIter;
        if (pExistingRecord) {
            if (!getArgumentAccessCode(
                    *argIter,
                    pExistingRecord,
                    existingArgs,
                    existingArrayArgs,
                    &from))
                return false;
        }

        pRecord->writeArgument(recordName, *argIter, from, &tempCode);
        out << "        " << tempCode << std::endl;
    }

    // set value for array args
    for (argIter = arrayArgs.begin(); argIter != arrayArgs.end(); argIter++) {
        std::string from = *argIter;
        if (pExistingRecord) {
            if (!getArgumentAccessCode(
                    *argIter,
                    pExistingRecord,
                    existingArgs,
                    existingArrayArgs,
                    &from))
                return false;
        }

        pRecord->writeArrayArgument(recordName, *argIter, from, &tempCode);
        out << "        " << tempCode << std::endl;
    }

    //==Print record serialization
    pRecord->serialize(recordName, serBufName, serLenName, &serCode);
    out << "        void * " << serBufName << " = NULL;" << std::endl
        << "        uint64_t " << serLenName << ";" << std::endl
        << "        " << serCode << std::endl;

    //==Clean up record and its description
    pRecord->deleteObject();

    return true;
}

//=============================
// printForwarding
//=============================
bool GeneratorBase::printForwarding(
    SpecificationNode node,
    std::string moduleAccessCode,
    std::ostream& out,
    bool avoidUid,
    int uidToAvoid,
    std::list<int>* pOutAvoidedCommIds,
    gti::I_RecordType* pRecord,
    std::list<std::string> args,
    std::list<std::string> arrayArgs,
    std::string avoidReducibleVar)
{
    SpecificationNode child, subchild, subsubchild;
    std::map<int, std::map<int, bool>>
        forwards; // maps uids to a list of (comm-id, isReducable) pairs
    std::map<int, std::map<int, bool>>::iterator forwardIter;

    //==Child: forwards
    // We start with the forwards, in order to know whether we need records necessarily or
    // optionally (whether they are only used for reducible forwards or not)
    child = node.findChildNodeNamedOrErr(
        "forwards",
        "|  |-->Error: a forwarding node has no \"forwards\" child.");
    if (!child)
        return false;

    //==Sub-Childs: forward
    // process forwarding
    subchild = child.findChildNodeNamed("forward");
    while (subchild) {
        int uid, commId;
        std::string uidString, commIdString, reducableStr;
        bool isReducable = false;

        //==child: record-uid
        subsubchild = subchild.findChildNodeNamedOrErr(
            "record-uid",
            "|  |-->Error: a forward node has no \"record-uid\" child.");
        if (!subsubchild)
            return false;
        uidString = subsubchild.getNodeContent();
        uid = atoi(uidString.c_str());

        //==child: comm-id
        subsubchild = subchild.findChildNodeNamedOrErr(
            "comm-id",
            "|  |-->Error: a forward node has no \"comm-id\" child.");
        if (!subsubchild)
            return false;
        commIdString = subsubchild.getNodeContent();
        commId = atoi(commIdString.c_str());

        //==Attribute: reducable
        if (subchild.getAttribute("reducable", &reducableStr)) {
            if (reducableStr != "yes" && reducableStr != "no") {
                std::cerr << "Error: a forward node specifies the \"reducable\" attribute with an "
                             "unknown value of \""
                          << reducableStr << "\"; valid values are \"yes\" and \"no\"."
                          << std::endl;
                return false;
            }

            if (reducableStr == "yes")
                isReducable = true;
        }

        if (!avoidUid || uid != uidToAvoid) {
            // Store the information on the target of the forward
            if (forwards.find(uid) == forwards.end()) {
                std::map<int, bool> commRed;
                commRed.insert(std::make_pair(commId, isReducable));
                forwards.insert(std::make_pair(uid, commRed));
            } else {
                forwards[uid].insert(std::make_pair(commId, isReducable));
            }
        } else {
            // add to list of commIds avoided
            if (pOutAvoidedCommIds)
                pOutAvoidedCommIds->push_back(commId);
        }

        // next
        subchild = subchild.findSiblingNamed("forward");
    }

    //==Print the records
    //==Child: records
    child = node.findChildNodeNamedOrErr(
        "records",
        "|  |-->Error: a forwarding node has no \"records\" child.");
    if (!child)
        return false;

    //==Sub-Childs: record
    subchild = child.findChildNodeNamed("record");
    while (subchild) {
        int uid;
        std::string freeCode;
        std::map<int, bool>::iterator commIter;
        bool hasIntra = false;

        std::string uidString;
        subchild.getAttribute("uid", &uidString);
        uid = atoi(uidString.c_str());

        // Find the forwards for the uid
        forwardIter = forwards.find(uid);
        if (forwardIter == forwards.end()) {
            std::cerr << " | -> Warning: there is a forward node with a record for uid " << uid
                      << " but this uid is not forwarded anywhere ..." << std::endl;
            subchild = subchild.findSiblingNamed("record");
            continue;
        }

        // Start timing (if profiling)
        if (myProfiling) {
            out << "uint64_t tStart_" << uid << " = " << moduleAccessCode << "getUsecTime();"
                << std::endl;
        }

        //==Determine reduction behavior for this record
        bool neededIfReduced = false;
        for (commIter = forwardIter->second.begin(); commIter != forwardIter->second.end();
             commIter++) {
            if (commIter->second == false) {
                neededIfReduced = true;
                break;
            }
        }

        if (!neededIfReduced) {
            // Start if that avoids creating this record if reduction was successful!
            if (avoidReducibleVar != "") {
                out << "      if (!" << avoidReducibleVar << ")" << std::endl
                    << "      {" << std::endl;
            }
        }

        //==Print the record
        if (!avoidUid || uid != uidToAvoid) {
            if (!printRecord(subchild, &uid, &freeCode, out, pRecord, args, arrayArgs))
                return false;
        }

        //==Print the Forwards
        // print reference count
        out << "        //reference count for uid " << uid << std::endl
            << "        int *referenceCount" << forwardIter->first << " = new int;" << std::endl
            << "        *referenceCount" << forwardIter->first << " = "
            << forwardIter->second.size() << ";" << std::endl
            << std::endl;

        // print the actual forwarding
        for (commIter = forwardIter->second.begin(); commIter != forwardIter->second.end();
             commIter++) {
            int commId = commIter->first;
            bool isReducable = commIter->second;
            int commIndex = 0;
            std::string commModeName = "myCStrats";
            bool isIntra = false, isDown = false;

            // Determine the comm mod and its index to use
            if (myCommMods.find(commId) != myCommMods.end()) {
                // Calculate index to use for this comm id
                std::map<int, std::string>::iterator indexIter;
                for (indexIter = myCommMods.begin(); indexIter != myCommMods.end();
                     indexIter++, commIndex++) {
                    if (indexIter->first == commId)
                        break;
                }
            } else if (myIntraCommMods.find(commId) != myIntraCommMods.end()) {
                // Calculate index to use for this comm id
                std::map<int, std::string>::iterator indexIter;
                for (indexIter = myIntraCommMods.begin(); indexIter != myIntraCommMods.end();
                     indexIter++, commIndex++) {
                    if (indexIter->first == commId)
                        break;
                }

                // Set name to intra modules
                commModeName = "myIntraCStrats";
                isIntra = true;
                hasIntra = true;
            } else if (myDownCommMods.find(commId) != myDownCommMods.end()) {
                // Calculate index to use for this comm id
                std::map<int, std::string>::iterator indexIter;
                for (indexIter = myDownCommMods.begin(); indexIter != myDownCommMods.end();
                     indexIter++, commIndex++) {
                    if (indexIter->first == commId)
                        break;
                }

                // Set name to intra modules
                commModeName = "myDownCStrats";
                isDown = true;
            } else {
                std::cerr
                    << "|  |-->Error: a forward node uses an invalid comm-id, specified was: \""
                    << commId
                    << "\", this neither refers to an inter nor to an intra communication module."
                    << std::endl;
                return false;
            }

            //==print the forwarding
            if (isReducable && avoidReducibleVar != "") {
                out << "      if (!" << avoidReducibleVar << ")" << std::endl
                    << "      {" << std::endl;
            }

            out << "            //Forward uid=" << uid << " to commId=" << commId << std::endl
                << "            " << moduleAccessCode << commModeName << "[" << commIndex << "]->";

            if (!isDown)
                out << "send(";
            else
                out << "broadcast(";

            if (isIntra) {
                // For intra communication we need to add the to place here
                out << "implicitToPlace, ";
            }

            out << "buf" << uid << ", buf" << uid << "Len, referenceCount" << uid
                << ", free_serialized_buf);" << std::endl
                << std::endl;

            if (isReducable && avoidReducibleVar != "") {
                out << "      }" << std::endl;
            }

        } // for comm-ids for current uid

        //==print the free record code
        out << "        //free record for uid " << uid << std::endl
            << "        " << freeCode << std::endl
            << std::endl;

        if (!neededIfReduced) {
            // Start if that avoids creating this record if reduction was successful!
            if (avoidReducibleVar != "") {
                out << "      }" << std::endl;
            }
        }

        // Stop timing (if profiling)
        if (myProfiling) {
            if (!hasIntra) // This should work right now as intra communicated events are not
                           // communicated upwards, if so we would need to switch reporting
                           // depending on the individual forwardings
                out << moduleAccessCode << "usec_communicating += " << moduleAccessCode
                    << "getUsecTime() - tStart_" << uid << ";" << std::endl
                    << moduleAccessCode << "count_communicating += 1;" << std::endl;
            else
                out << moduleAccessCode << "usec_communicatingIntra += " << moduleAccessCode
                    << "getUsecTime() - tStart_" << uid << ";" << std::endl
                    << moduleAccessCode << "count_communicatingIntra += 1;" << std::endl;
        }

        // next record
        subchild = subchild.findSiblingNamed("record");
    } // For record sub-childs

    return true;
}

//=============================
// getArgumentAccessCode
//=============================
bool GeneratorBase::getArgumentAccessCode(
    std::string argName,
    gti::I_RecordType* pRecord,
    std::list<std::string> args,
    std::list<std::string> arrayArgs,
    std::string* pOutCode)
{
    std::list<std::string>::iterator argIter;

    // Search the argument and create the code to return it
    for (argIter = args.begin(); argIter != args.end(); argIter++) {
        if (*argIter == argName) {
            if (pRecord->returnArgument(myGetRecordName(), argName, pOutCode) != GTI_SUCCESS)
                return false;
            return true;
        }
    }
    for (argIter = arrayArgs.begin(); argIter != arrayArgs.end(); argIter++) {
        if (*argIter == argName) {
            if (pRecord->returnArrayArgument(myGetRecordName(), argName, pOutCode) != GTI_SUCCESS)
                return false;
            return true;
        }
    }

    return false;
}

//=============================
// getProfilingVariableName
//=============================
std::string GeneratorBase::getProfilingVariableName(AnalysisFunction* fn)
{
    return (std::string) "usec_" + fn->info->name + "_" + fn->function;
}

//=============================
// getProfilingCountVariableName
//=============================
std::string GeneratorBase::getProfilingCountVariableName(AnalysisFunction* fn)
{
    return (std::string) "count_" + fn->info->name + "_" + fn->function;
}

/*EOF*/
