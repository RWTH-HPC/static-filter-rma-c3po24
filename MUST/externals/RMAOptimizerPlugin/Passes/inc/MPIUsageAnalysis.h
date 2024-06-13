#pragma once

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/DependenceAnalysis.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/PassManager.h"
#include <map>
#include <vector>
#include <set>

namespace llvm {

class MPIUsageAnalysis : public AnalysisInfoMixin<MPIUsageAnalysis> {
    public:
        static llvm::AnalysisKey Key;

        //Result Type
        struct Result {
            std::set<Function*> UnsafeFunctions;
            #warning HACK: Never invalidate MPIUsageAnalysis result
            bool invalidate(Module &M, const PreservedAnalyses &PA, ModuleAnalysisManager::Invalidator &) { return false; }
        } typedef MPIUsageResult;

        // Run Analysis
        Result run(Module &M, ModuleAnalysisManager &AM);

    private:
        // Result Store
        MPIUsageResult ResultVal;

        void iterate(Module& M, std::set<Function*>& Unsafe);
};

} // namespace llvm
