#pragma once

#include "llvm/IR/PassManager.h"

namespace llvm {

class TSanActivatorPass : public PassInfoMixin<TSanActivatorPass> {
    public:
        PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
    
    private:
        const std::vector<std::string> ActivatorCalls = {
            "MPI_Win_create",
            "MPI_Win_allocate",
        };
};

} // namespace llvm
