#include "../inc/ClusterModuleShim.h"
#include "../inc/MPIUsageAnalysis.h"

#include "llvm/Passes/PassPlugin.h"
#include "llvm/Passes/PassBuilder.h"
#include <llvm/IR/PassManager.h>
#include <llvm/Transforms/Instrumentation.h>

using namespace llvm;

PreservedAnalyses ClusterModuleShim::run(Module &M,
                                         ModuleAnalysisManager &AM) {
    AM.getResult<MPIUsageAnalysis>(M);
    return PreservedAnalyses::all();
}
