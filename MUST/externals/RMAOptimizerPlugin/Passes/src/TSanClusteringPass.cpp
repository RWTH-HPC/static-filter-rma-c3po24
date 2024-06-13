#include "../inc/TSanClusteringPass.h"

#include "../inc/MPIUsageAnalysis.h"

#include "llvm/Passes/PassPlugin.h"
#include "llvm/Passes/PassBuilder.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"

#include <llvm/IR/InstrTypes.h>
#include <llvm/Support/ErrorHandling.h>
#include <regex>
#include <vector>

#define DEBUG_TYPE "tsanMOD-clustering"

using namespace llvm;

STATISTIC(NumRemovedInstr, "Number of removed ThreadSanitizer Calls by Clustering");

PreservedAnalyses TSanClusteringPass::run(Function &F,
                                         FunctionAnalysisManager &FAM) {
    errs() << "Running TSanClusteringPass on Function " << F.getName() << "\n";
    MPIUsageAnalysis::Result* UnsafeFuncs = FAM.getResult<ModuleAnalysisManagerFunctionProxy>(F).getCachedResult<MPIUsageAnalysis>(*F.getParent());
    if (!UnsafeFuncs) {
        errs() << "Could not get info on unsafe functions! Conservative Analysis may cause slowdown during runtime\n";
    }
    std::list<std::vector<TSanCall>> TSanClusters;
    for (BasicBlock& B : F) {
        for (Instruction& I : B) {
            if(CallBase* FuncCall = dyn_cast<CallBase>(&I)) {
                std::string FuncName;
                if (!FuncCall->getCalledFunction()) {
                    if (!FuncCall->getCalledOperand()->getName().empty()) {
                        FuncName = FuncCall->getCalledOperand()->getName().data();
                    } else {
                        continue;
                    }
                } else {
                    FuncName = FuncCall->getCalledFunction()->getName().data();
                }

                bool IdentifiedType = false;
                // Check if call is ThreadSanitizer Call
                for (std::pair<std::string,std::pair<TSanCallType,int>> InstCall : TSanInstCalls) {
                    if (std::regex_match(FuncName,std::regex(InstCall.first))) {
                        TSanCall CurTSanCall = {FuncCall, InstCall.second.first, InstCall.second.second};
                        errs() << "Detected ThreadSanitizer instrumented call '" << FuncName << "' in '" << FuncCall->getParent()->getParent()->getName() << "' of size " << InstCall.second.second << "\n";
                        bool found = false;
                        IdentifiedType = true;
                        for (std::vector<TSanCall>& Cluster : TSanClusters) {
                            if (Cluster[0].callValue->getArgOperand(0) == FuncCall->getArgOperand(0)) {
                                errs() << "Current call matches a cluster signature!\n";
                                Cluster.push_back(CurTSanCall);
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            errs() << "Current call requires a new cluster\n";
                            TSanClusters.push_back(std::vector<TSanCall>{CurTSanCall});
                        }
                        break;
                    }
                }
                // Check if call is unsafe
                if (!IdentifiedType && (std::regex_match(FuncName,std::regex(".*(MPI|mpi)_.*")) || (UnsafeFuncs && UnsafeFuncs->UnsafeFunctions.count(FuncCall->getCalledFunction())))) {
                    errs() << "Encountered '" << FuncName << "', which is unsafe. Flushing clusters...\n";
                    IdentifiedType = true;
                    FlushClusters(TSanClusters);
                }
                if (!IdentifiedType && !UnsafeFuncs) {
                    errs() << "Encountered unidentified function, which may be unsafe. Flushing clusters...\n";
                    errs() << "NOTE: This would not happen if MPIUsageAnalysis were queried correctly!\n";
                    FlushClusters(TSanClusters);
                }
            }
        }
        errs() << "BasicBlock has ended. Flushing clusters...\n";
        FlushClusters(TSanClusters);
    }
    errs() << "TSanClusteringPass done!\n";
    return PreservedAnalyses::all();
}

void TSanClusteringPass::FlushClusters(std::list<std::vector<TSanCall>>& TSanClusters) {
    for (std::vector<TSanCall> Cluster : TSanClusters) {
        if (Cluster.size() <= 1) continue;
        std::vector<TSanCall> subClusterRead;
        std::vector<TSanCall> subClusterWrite;
        // Generate subclusters
        for (TSanCall call : Cluster) {
            if (call.type == TSanCallType::TSanWriteInst) {
                subClusterWrite.push_back(call);
            } else if (call.type == TSanCallType::TSanReadInst) {
                subClusterRead.push_back(call);
            } else {
                llvm_unreachable("Unkown TSan call type!");
            }
        }
        // Determine max R/W size
        int readMax = determineMaxAccessSize(subClusterRead);
        int writeMax = determineMaxAccessSize(subClusterWrite);
        NumRemovedInstr += reduceConsistentCluster(subClusterWrite,writeMax);
        NumRemovedInstr += reduceConsistentCluster(subClusterRead,writeMax >= readMax ? -1 : readMax);
    }
    TSanClusters.clear();
}

int TSanClusteringPass::determineMaxAccessSize(std::vector<TSanCall> Cluster) {
    int max = 0;
    for (TSanCall call : Cluster) {
        max = std::max(call.accessSize,max);
    }
    return max;
}

int TSanClusteringPass::reduceConsistentCluster(std::vector<TSanCall> Cluster, int maxSize) {
    if (Cluster.size() <= 1) return 0; // Can't reduce unitary cluster
    bool foundMax = maxSize == -1 ? true : false; // maxSize == -1 indicates full deletion of cluster requested
    int countRemoved = 0;
    for (TSanCall call : Cluster) {
        if (!foundMax && call.accessSize == maxSize) { foundMax = true; continue; }
        call.callValue->eraseFromParent();
        countRemoved++;
    }
    return countRemoved;
}