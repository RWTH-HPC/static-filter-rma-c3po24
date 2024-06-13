#include "../inc/TSanActivatorPass.h"

#include "llvm/Passes/PassPlugin.h"
#include "llvm/Passes/PassBuilder.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;

PreservedAnalyses TSanActivatorPass::run(Function &F,
                                         FunctionAnalysisManager &AM) {
    Module* M = F.getParent();
    LLVMContext& Ctx = M->getContext();
    FunctionCallee activator_decl = M->getOrInsertFunction("__tsan_start_racedetect", Type::getVoidTy(Ctx));
    Value* activator = activator_decl.getCallee();
    if (!F.hasFnAttribute(Attribute::SanitizeThread) && !F.hasFnAttribute(Attribute::DisableSanitizerInstrumentation)) {
        errs() << "Added Function '" << F.getName() << "' to be instrumented\n";
        F.addFnAttr(Attribute::SanitizeThread);
    }
    for (BasicBlock &B : F) {
        for (Instruction &I : B) {
            if (CallBase *FuncCall = dyn_cast<CallBase>(&I)) {
                if (!FuncCall->getCalledFunction()) continue;
                if (std::find(ActivatorCalls.begin(), ActivatorCalls.end(), FuncCall->getCalledFunction()->getName()) != ActivatorCalls.end())  {
                    CallInst* AnalysisStarter = CallInst::Create(activator_decl.getFunctionType(), activator, "", FuncCall);
                }
            }
        }
    }
    return PreservedAnalyses::all();
}
