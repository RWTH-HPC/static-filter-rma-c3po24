#include "../inc/MPIUsageAnalysis.h"

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/DependenceAnalysis.h"
#include "llvm/Analysis/IVDescriptors.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Transforms/IPO/Attributor.h"

#include <algorithm>
#include <cstddef>
#include <regex>
#include <ostream>
#include <string>
#include <utility>
#include <vector>
#include <map>

using namespace llvm;

AnalysisKey MPIUsageAnalysis::Key;

MPIUsageAnalysis::MPIUsageResult MPIUsageAnalysis::run(Module &M, ModuleAnalysisManager &AM) {
    std::set<Function*> Unsafe_Cur;
    std::set<Function*> Unsafe_Last;
    do {
        Unsafe_Last = Unsafe_Cur;
        iterate(M, Unsafe_Cur);
    } while (Unsafe_Cur != Unsafe_Last);
    ResultVal.UnsafeFunctions = std::move(Unsafe_Cur);
    return ResultVal;
}

void MPIUsageAnalysis::iterate(Module& M, std::set<Function*>& Unsafe) {
    for (Function& F : M) {
        for (BasicBlock& B : F) {
            for (Instruction& I : B) {
                if(CallBase* FuncCall = dyn_cast<CallBase>(&I)) {
                    std::string FuncName;
                    if (!FuncCall->getCalledFunction()) {
                        if (!FuncCall->getCalledOperand()->getName().empty()) {
                            FuncName = FuncCall->getCalledOperand()->getName().data();
                        } else {
                            errs() << "Encountered unclear function call! Beware of issues\n";
                            continue;
                        }
                    } else {
                        FuncName = FuncCall->getCalledFunction()->getName().data();
                    }

                    if (std::regex_match(FuncName,std::regex(".*(MPI|mpi)_.*")) || Unsafe.count(FuncCall->getCalledFunction())) {
                        Unsafe.insert(&F);
                    }
                }
            }
        }
    }
}