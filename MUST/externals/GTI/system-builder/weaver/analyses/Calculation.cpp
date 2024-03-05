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
 * @file Calculation.cpp
 * 		@see gti::weaver::Calculation
 *
 * @author Tobias Hilbrich
 * @date 12.01.2010
 */

#include <assert.h>
#include <iostream>
#include <stdio.h>

#include "Calculation.h"
#include "Analyses.h"

using namespace gti;
using namespace gti::weaver::analyses;

//=============================
// Constructor
//=============================
Calculation::Calculation() : myName(""), myCallMappings(), myArgumentSpec(), myGroup(NULL)
{
    /*Nothing to do.*/
}

//=============================
// Constructor
//=============================
Calculation::Calculation(
    std::string name,
    std::vector<InputDescription*> argumentSpec,
    AnalysisGroup* group)
    : myName(name), myCallMappings(), myArgumentSpec(argumentSpec), myGroup(group)
{
    if (group) {
        if (!group->registerCalculation(this)) {
            std::cerr << "Error: duplicate calculation with name \"" << name
                      << "\" exists in the analysis group \"" << group->getGroupName() << "\""
                      << std::endl;
        }
    }
}

//=============================
// Destructor
//=============================
Calculation::~Calculation()
{
    // mappings
    for (auto* myCallMapping : myCallMappings) {
        if (myCallMapping)
            delete myCallMapping;
    }
    myCallMappings.clear();

    // argument specification
    for (auto* i : myArgumentSpec) {
        if (i)
            delete i;
    }
    myArgumentSpec.clear();

    // group pointer
    myGroup = NULL;
}

//=============================
// addCallMapping
//=============================
bool Calculation::addCallMapping(Mapping* add_object)
{
    assert(add_object);

    // validate mapping against input specification
    std::vector<Input*> inputs = add_object->getArgumentInputs();

    if (inputs.size() != myArgumentSpec.size()) {
        std::cerr << "Error: an invalid mapping of call \"" << add_object->getApiCall()->getName()
                  << "\" to analysis \"" << getName() << "\" was attempted, the analysis needs "
                  << myArgumentSpec.size() << " inputs, while the mapping uses " << inputs.size()
                  << " inputs." << std::endl;
        return false;
    }

    for (std::size_t i = 0; i < myArgumentSpec.size(); i++) {
        if (inputs[i]->getType() != myArgumentSpec[i]->getArgumentType()) {
            // create a warning and store it in analyses
            //(We can't realy know whether two C++ types are the same after all)
            Analyses::getInstance()->addTypeMissmatchWarning(
                inputs[i]->getType(),
                myArgumentSpec[i]->getArgumentType(),
                "Mapping of calculation \"" + getName() + "\" to call \"" +
                    add_object->getApiCall()->getName() + "\"");
        }
    }

    // add
    myCallMappings.push_back(add_object);

    return true;
}

//=============================
// getCallMappings
//=============================
std::vector<Mapping*> Calculation::getCallMappings() { return myCallMappings; }

//=============================
// addArgumentSpec
//=============================
void Calculation::addArgumentSpec(InputDescription* add_object)
{
    myArgumentSpec.push_back(add_object);
}

//=============================
// getArgumentSpec
//=============================
std::vector<InputDescription*> Calculation::getArgumentSpec() { return myArgumentSpec; }

//=============================
// setGroup
//=============================
void Calculation::setGroup(AnalysisGroup* new_var) { myGroup = new_var; }

//=============================
// getGroup
//=============================
AnalysisGroup* Calculation::getGroup() { return myGroup; }

//=============================
// setName
//=============================
void Calculation::setName(std::string new_var) { myName = new_var; }

//=============================
// getName
//=============================
std::string Calculation::getName() { return myName; }

//=============================
// print
//=============================
std::ostream& Calculation::print(std::ostream& out) const
{
    out << "name=" << myName << ", "
        << "arguments={";

    for (std::size_t i = 0; i < myArgumentSpec.size(); i++) {
        if (i > 0)
            out << ", ";
        out << *(myArgumentSpec[i]);
    }

    out << "}, #ofMappings=" << myCallMappings.size();

    return out;
}

//=============================
// toDotNode
//=============================
std::string Calculation::toDotNode(void)
{
    std::string ret = "";

    std::string color = getDotNodeColor();

    ret += getDotName() + "[label=\"{" + getName() + "|{";

    for (std::size_t iArg = 0; iArg < myArgumentSpec.size(); iArg++) {
        if (iArg != 0)
            ret += "|";
        char temp[32];
        sprintf(temp, "%ld", iArg);
        ret += std::string("<Arg") + temp + ">" + (myArgumentSpec[iArg])->getName() + ":" +
               (myArgumentSpec[iArg])->getArgumentType();
    }

    ret += "}}\", shape=Mrecord, fillcolor=" + color + ", style=filled];";

    return ret;
}

//=============================
// toDotNodeBottomUp
//=============================
std::string Calculation::toDotNodeBottomUp(void)
{
    std::string ret = "";

    std::string color = getDotNodeColor();

    ret += getDotName() + "[label=\"{{";

    for (std::size_t iArg = 0; iArg < myArgumentSpec.size(); iArg++) {
        if (iArg != 0)
            ret += "|";
        char temp[32];
        sprintf(temp, "%ld", iArg);
        ret += std::string("<Arg") + temp + ">" + (myArgumentSpec[iArg])->getName() + ":" +
               (myArgumentSpec[iArg])->getArgumentType();
    }

    ret += "}|" + getName() + "}\", shape=Mrecord, fillcolor=" + color + ", style=filled];";

    return ret;
}

//=============================
// getDotName
//=============================
std::string Calculation::getDotName(void)
{
    std::string ret = getName();

    size_t pos = 0;
    while (ret.find_first_of(':') != ret.npos) {
        pos = ret.find_first_of(':');
        ret.replace(pos, 1, "_");
    }

    return ret;
}

//=============================
// hasMappingForCall
//=============================
bool Calculation::hasMappingForCall(std::string callName, std::string apiName)
{
    for (auto* myCallMapping : myCallMappings) {
        Call* c = myCallMapping->getApiCall();
        if (c->getName() == callName && c->getGroup()->getApiName() == apiName)
            return true;
    }

    return false;
}

//=============================
// hasMappingForCall
//=============================
bool Calculation::hasMappingForCall(std::string callName, std::string apiName, int id)
{
    for (auto* myCallMapping : myCallMappings) {
        Call* c = myCallMapping->getApiCall();
        if (c->getName() == callName && c->getGroup()->getApiName() == apiName &&
            myCallMapping->getId() == id)
            return true;
    }

    return false;
}

//=============================
// hasMappingForCall
//=============================
bool Calculation::hasMappingForCall(
    std::string callName,
    std::string apiName,
    int id,
    CalculationOrder order)
{
    for (auto* myCallMapping : myCallMappings) {
        if (myCallMapping->getOrder() != order)
            continue;

        Call* c = myCallMapping->getApiCall();
        if (c->getName() == callName && c->getGroup()->getApiName() == apiName &&
            myCallMapping->getId() == id)
            return true;
    }

    return false;
}

//=============================
// getMappingsForCall
//=============================
std::list<Mapping*> Calculation::getMappingsForCall(Call* call)
{
    std::list<Mapping*> ret;

    for (auto* myCallMapping : myCallMappings) {
        Call* c = myCallMapping->getApiCall();
        if (call == c)
            ret.push_back(myCallMapping);
    }

    return ret;
}

//=============================
// getMappingForCall
//=============================
Mapping* Calculation::getMappingForCall(Call* call, int id)
{
    for (auto* myCallMapping : myCallMappings) {
        Call* c = myCallMapping->getApiCall();
        if (call == c && myCallMapping->getId() == id)
            return myCallMapping;
    }

    return NULL;
}

/*EOF*/
