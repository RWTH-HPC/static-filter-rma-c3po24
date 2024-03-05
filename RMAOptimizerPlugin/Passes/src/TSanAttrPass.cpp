#include "../inc/TSanAttrPass.h"

#include "llvm/Passes/PassPlugin.h"
#include "llvm/Passes/PassBuilder.h"
#include <llvm/Transforms/Instrumentation.h>

using namespace llvm;

PreservedAnalyses TSanAttrPass::run(Function &F,
                                         FunctionAnalysisManager &AM) {
    F.addFnAttr(Attribute::SanitizeThread);
    return PreservedAnalyses::all();
}
