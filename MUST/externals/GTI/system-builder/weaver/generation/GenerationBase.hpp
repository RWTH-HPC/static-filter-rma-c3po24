using namespace gti::weaver::generation;

//=============================
// printRecord
//=============================
template <typename I>
void GenerationBase::printRecord(Call* call, int uid, I inputs)
{
    out << "\t\t\t\t\t\t<record uid=\"" << uid << "\">" << std::endl;

    // add a "element"/"array-element" node for each input
    for (auto inputIter = inputs.begin(); inputIter != inputs.end(); inputIter++) {
        Input* input = *inputIter;
        if (input->isArrayInput())
            out << "\t\t\t\t\t\t\t<array-element>" << std::endl;
        else
            out << "\t\t\t\t\t\t\t<element>" << std::endl;

        out << "\t\t\t\t\t\t\t\t<name>" << input->getName() << "</name>" << std::endl
            << "\t\t\t\t\t\t\t\t<type>" << input->getType() << "</type>" << std::endl
            << "\t\t\t\t\t\t\t\t<from-call>" << call->getName() << "</from-call>" << std::endl
            << "\t\t\t\t\t\t\t\t<as-arg>" << input->getName() << "</as-arg>" << std::endl;

        if (input->isArrayInput()) {
            out << "\t\t\t\t\t\t\t\t<length-argument>" << input->getLenName()
                << "</length-argument>" << std::endl
                << "\t\t\t\t\t\t\t</array-element>" << std::endl;
        } else {
            out << "\t\t\t\t\t\t\t</element>" << std::endl;
        }
    }

    out << "\t\t\t\t\t\t</record>" << std::endl;
}
