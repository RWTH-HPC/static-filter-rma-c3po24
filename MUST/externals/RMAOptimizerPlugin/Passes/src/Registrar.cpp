#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/OptimizationLevel.h>

#include "../inc/TSanActivatorPass.h"
#include "../inc/TSanRMAOptimizer.h"
#include "../inc/MPIUsageAnalysis.h"
#include "../inc/TSanClusteringPass.h"
#include "../inc/TSanAttrPass.h"
#include "../inc/ClusterModuleShim.h"
#include "../inc/TSanInstrFilter.h"

#include "../inc/ThreadSanitizerMOD.h"

using namespace llvm;

bool FPMHook(StringRef Name, FunctionPassManager &FPM,
             ArrayRef<PassBuilder::PipelineElement>) {
    if (Name == "tsanMOD") {
        FPM.addPass(ThreadSanitizerMODPass());
        return true;
    }
    if (Name == "tsanMOD-activatoropt") {
        FPM.addPass(TSanActivatorPass());
        return true;
    }
    if (Name == "tsanMOD-clusteringopt") {
        FPM.addPass(TSanClusteringPass());
        return true;
    }
    if (Name == "tsanMOD-attrpass") {
        FPM.addPass(TSanAttrPass());
        return true;
    }
    return false;
};

bool MPMHook(StringRef Name, ModulePassManager &MPM,
             ArrayRef<PassBuilder::PipelineElement>) {
    if (Name == "tsanMOD-module") {
        MPM.addPass(ModuleThreadSanitizerMODPass());
        return true;
    }
    if (Name == "tsanMOD-cluster-shim") {
        MPM.addPass(ClusterModuleShim());
        return true;
    }
    return false;
};

void MAMHook(ModuleAnalysisManager &MAM) {
    MAM.registerPass([&] { return TSanRMAOptimizerAnalysis(); });
    MAM.registerPass([&] { return MPIUsageAnalysis(); });
    MAM.registerPass([&] { return TSanInstrFilter(); });
};

void PBHook(PassBuilder &PB) {
    PB.registerPipelineParsingCallback(FPMHook);
    PB.registerPipelineParsingCallback(MPMHook);
    PB.registerAnalysisRegistrationCallback(MAMHook);
}

llvm::PassPluginLibraryInfo getRMAOptimizerPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "RMAOptimizerPlugin",
          LLVM_VERSION_STRING, PBHook};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getRMAOptimizerPluginInfo();
}
