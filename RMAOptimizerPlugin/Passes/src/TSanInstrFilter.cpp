#include "../inc/TSanInstrFilter.h"

#include <llvm/Support/CommandLine.h>
#include <llvm/Demangle/Demangle.h>

#include <algorithm>
#include <cstddef>
#include <fstream>
#include <regex>
#include <ostream>
#include <string>
#include <utility>
#include <vector>
#include <map>

using namespace llvm;

AnalysisKey TSanInstrFilter::Key;

static cl::opt<std::string> ClFilterFilePath(
    "tsanMOD-filter-path", cl::init("_None"),
    cl::desc("Provide a filtering file for a allowlist/denylist of functions for TSan Instrumentation"),
    cl::Hidden);

TSanInstrFilter::TSanInstrFilterResult TSanInstrFilter::run(Module &M, ModuleAnalysisManager &AM) {
    this->cur_module = &M;
    if (ClFilterFilePath == "_None") {
        errs() << "Requested TSan Instrumentation Filter, but did not provide a file path!\n";
        return ResultVal;
    }
    errs() << "TSan Instrumentation Filter File used: '" << ClFilterFilePath << "'\n";

    std::ifstream filter_file(ClFilterFilePath);

    std::string line;
    bool err;
    int line_num = 0;
    while (std::getline(filter_file, line)) {
        line_num++;
        err = parseFileByLine(line);
        if (err) {
            errs() << "Could not parse line " << line_num << ": '" << line << "'\n";
            ResultVal.FunctionFilterList.clear();
            ResultVal.allowlist = false;
            break;
        }
    }

    return ResultVal;
}

bool TSanInstrFilter::parseFileByLine(std::string line) {
    if (line == "ALLOWLIST") {
        ResultVal.allowlist = true;
        return false;
    } else if (line == "DENYLIST") {
        ResultVal.allowlist = false;
        return false;
    }

    else if (std::regex_match(line,std::regex("FUNC::.*"))) {
        std::string FuncName = line.substr(6);
        bool found = false;
        for (Function& F : cur_module->functions()) {
            std::string demangled_Func = llvm::demangle(F.getName().str());
            if (demangled_Func == FuncName || demangled_Func == FuncName + "()") {
                if (F.getName() != FuncName)
                    errs() << "Warning: Applied Filter to mangled function: '" << F.getName() << "' demangled to '" << demangled_Func << "'\n";
                found = true;
                ResultVal.FunctionFilterList.insert(&F);
            }
        }
        if (!found) {
            errs() << "Did not find a suitable function for filter entry: '" << FuncName << "'\n";
            return true;
        }
        errs() << "Processed entry '" << FuncName << "'\n";
        return false;
    }

    return true;
}