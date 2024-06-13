#pragma once

#include "llvm/IR/PassManager.h"

#include <vector>
#include <set>

namespace llvm {

class TSanInstrFilter : public AnalysisInfoMixin<TSanInstrFilter> {
    public:
        static llvm::AnalysisKey Key;

        //Result Type
        struct Result {
            // List of functions to filter
            std::set<Function*> FunctionFilterList;
            // Treat input as allowlist or denylist - Allowlist: Only filter functions in list, Denylist: Filter all not in list.
            bool allowlist;
            bool invalidate(Module &M, const PreservedAnalyses &PA, ModuleAnalysisManager::Invalidator &) { return false; }
        } typedef TSanInstrFilterResult;

        // Run Analysis
        Result run(Module &M, ModuleAnalysisManager &AM);

    private:
        // Result Store
        TSanInstrFilterResult ResultVal;

        // Read a line of the filter file and do the requested changes
        bool parseFileByLine(std::string line);

        llvm::Module* cur_module;
};

} // namespace llvm
