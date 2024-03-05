
#define CONTAINS(c, i) (c.find(i) != c.end())
using namespace gti::weaver::layout;
//=============================
// addUsedArgs
//=============================
template <typename I>
void CallProperties::addUsedArgs(const I& args)
{
    myInformationRequired = true;

    // Adds to both lists
    addArgs(args, &myUsedArgs);
    addArgs(args, &myArgsToReceive);
}

//=============================
// addArgsToReceive
//=============================
template <typename I>
void CallProperties::addArgsToReceive(const I& args)
{
    myInformationRequired = true;

    addArgs(args, &myArgsToReceive);
}

//=============================
// addArgs
//=============================
template <typename I>
void CallProperties::addArgs(const I& args, InputSet* target)
{
    //  target->insert(args.begin(), args.end());
    for (auto arg : args)
        target->insert(arg);
}

template <typename I>
void CallProperties::addArgs(const I& args, InputList* target)
{
    for (auto arg : args)
        if (!CONTAINS(myUsedArgNames, arg->getName())) {
            target->push_back(arg);
            myUsedArgNames.insert(arg->getName());
        }
}

//=============================
// usedOrReceiveArgsContain
//=============================
template <typename I>
bool CallProperties::usedOrReceiveArgsContain(const I& other)
{
    /**
     * Build a super list of all inputs we use in this property, this includes:
     * - myUsedArgs
     * - myArgsToReceive
     * - any input a mapped operation uses
     */
    std::set<std::string> argNames{}, argLengthNames{}, opArgNames{}, opArgLengthNames{};

    for (auto arg : myUsedArgs) {
        argNames.insert(arg->getName());
        if (arg->isArrayInput())
            argLengthNames.insert(arg->getLenName());
    }
    for (auto arg : myArgsToReceive) {
        argNames.insert(arg->getName());
        if (arg->isArrayInput())
            argLengthNames.insert(arg->getLenName());
    }

    for (auto opIter : myOpsToExecute) {
        Mapping* m = opIter.first->getMappingForCall(myCall, opIter.second);
        if (m)
            for (auto tInputs : m->getArgumentInputs()) {
                opArgNames.insert(tInputs->getName());
                if (tInputs->isArrayInput())
                    opArgLengthNames.insert(tInputs->getLenName());
            }
    }

    /**
     * Now compare the given inputs to the super list of all directly and
     * indirectly used inputs
     */
    for (auto oIter : other) {
        if (CONTAINS(argNames, oIter->getName()))
            continue;
        if (CONTAINS(argLengthNames, oIter->getName()))
            continue;
        if (CONTAINS(opArgNames, oIter->getName()) || CONTAINS(opArgLengthNames, oIter->getName()))
            continue;
        if (oIter->isArrayInput() &&
            (CONTAINS(opArgNames, oIter->getLenName()) || CONTAINS(argNames, oIter->getLenName())))
            continue;
        Operation* op;
        int id;
        // Alternative is: the input requires an operation and we have this operation in our ops to
        // execute
        if (oIter->needsOperation(&op, &id)) {
            bool found = false;
            for (auto opIter : myOpsToExecute)
                if (opIter.first == op && opIter.second == id) {
                    found = true;
                    break;
                }
            if (found)
                continue;
        }
        return false;
    }
    return true;
}
