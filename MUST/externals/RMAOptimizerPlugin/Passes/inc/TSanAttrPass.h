#pragma once

#include "llvm/IR/PassManager.h"

namespace llvm {

class TSanAttrPass : public PassInfoMixin<TSanAttrPass> {
    public:
        PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};

} // namespace llvm
