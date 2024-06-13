#pragma once

#include "llvm/IR/PassManager.h"

namespace llvm {

class ClusterModuleShim : public PassInfoMixin<ClusterModuleShim> {
    public:
        PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
};

} // namespace llvm
