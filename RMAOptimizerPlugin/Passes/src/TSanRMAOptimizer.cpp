#include "../inc/TSanRMAOptimizer.h"

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
#include <optional>
#include <regex>
#include <string>
#include <utility>
#include <vector>
#include <map>

using namespace llvm;

static const cl::opt<int> ClAllowlistGenDepth(
    "tsanMOD-allowlist-depth", cl::init(9), // Experimentally found value
    cl::desc("Overwrite default RMAOptimizer allowlist depth"),
    cl::Hidden);

#define DefaultConfig {true, ClAllowlistGenDepth, ClAllowlistGenDepth == 0, true}

AnalysisKey TSanRMAOptimizerAnalysis::Key;

static const Function *getParent(const Value *V) {
    if (const Instruction *inst = dyn_cast<Instruction>(V)) {
    if (!inst->getParent())
        return nullptr;
    return inst->getParent()->getParent();
    }
 
    if (const Argument *arg = dyn_cast<Argument>(V))
        return arg->getParent();
 
    return nullptr;
}
 
static bool notDifferentParent(const Value *O1, const Value *O2) {
 
    const Function *F1 = getParent(O1);
    const Function *F2 = getParent(O2);
 
    return !F1 || !F2 || F1 == F2;
}

static std::optional<std::string> getFuncName(CallBase* FuncCall) {
    if (!FuncCall->getCalledFunction()) {
        if (!FuncCall->getCalledOperand()->getName().empty()) {
            return FuncCall->getCalledOperand()->getName().data();
        } else {
            return std::nullopt;
        }
    }
    return FuncCall->getCalledFunction()->getName().data();
}

TSanRMAOptimizerAnalysis::TSanRMAOptResult TSanRMAOptimizerAnalysis::run(Module &M,
                                                                         ModuleAnalysisManager &AM) {
    errs() << "Running on Module: " << M.getName() << "\n";

    std::map<RMAFramework,bool> FrameworksUsed;
    for (std::pair<std::string,RMAFramework> FWinit : FrameworkInitializers) {
        if (FrameworksUsed.count(FWinit.second) == 0) FrameworksUsed[FWinit.second] = false;
        FrameworksUsed[FWinit.second] = M.getFunction(FWinit.first) ? true : FrameworksUsed[FWinit.second];
    }

    errs() << "\n";
    errs() << "Detected RMA Models used: \n";
    for (std::pair<RMAFramework,bool> RMAUsage : FrameworksUsed) {
        errs() << RMAUsage.first << ": " << (RMAUsage.second ? "Y\n" : "N\n");
    }
    errs() << "\n";

    bool remoteisreadonly = true;
    bool remoteiswriteonly = true;

    errs() << "Phase 1: Factory Detection and Incidence relation up to depth " << ClAllowlistGenDepth << "\n";
    AAResults* AA;
    for (Function& F : M) {
        if (F.isDeclaration()) continue;
        AA = &AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager().getResult<AAManager>(F);
        for (BasicBlock& B : F) {
            for (Instruction& I : B) {
                if(CallBase* FuncCall = dyn_cast<CallBase>(&I)) {
                    std::optional<std::string> FuncNameOpt = getFuncName(FuncCall);
                    if (!FuncNameOpt)
                        continue;
                    std::string FuncName = FuncNameOpt.value();
                    for (std::pair<std::string,std::pair<std::set<int>,ShResType>> ShResFactory : SharedResourceFactories) {
                        if (std::regex_match(FuncName,std::regex(ShResFactory.first))) {
                            errs() << "Found Shared Resource Factory: '" << FuncName << "' called in '" << F.getName() <<"'\n";
                            remoteisreadonly = ShResFactory.second.second == ShResType::WriteBuf ? false : remoteisreadonly;
                            remoteiswriteonly = ShResFactory.second.second == ShResType::ReadBuf ? false : remoteiswriteonly;
                            Value* V;
                            std::set<int> ParamIndices = ShResFactory.second.first;
                            for (int index : ParamIndices) {
                                if (index >= 0) { // ShRes is parameter of func call
                                    V = FuncCall->getArgOperand(index);
                                } else if (index == -1) { // ShRes is return value of func call
                                    V = FuncCall;
                                }
                                SharedResource ShRes = {
                                    V,
                                    ShResFactory.second.second,
                                    0
                                };
                                int count = recurseGenerateAllowlist(ShRes, DefaultConfig, *AA);
                                errs() << "Identified " << count << " connected Value(s)\n";
                            }
                            break;
                        }
                    }
                }
            }
        }
    }
    errs() << "\n";


    if (FrameworksUsed[RMAFramework::OpenSHMEM]) {
        errs() << "Phase 2-OpenSHMEM: Adding all global Variables\n";
        for (GlobalVariable& GVar : M.getGlobalList()) {
            SharedResource G = {&GVar, ShResType::RemoteBuf};
            RecurseValue RV = {DefaultConfig, G};
            recurseGenerateAllowlist(G, DefaultConfig, *AA);
        }
    } else {
        errs() << "Phase 2-Default: Clipping unrelated global Variables\n";
        int clipped = 0;
        for (GlobalVariable& GVar : M.getGlobalList()) {
            SharedResource G = {&GVar, ShResType::DirtyBuf};
            if (std::find(SharedResources.begin(), SharedResources.end(), G) == SharedResources.end()) {
                GVar.addAttribute("no_sanitize", "thread");
                clipped++;
            }
        }
        errs() << "Clipped " << clipped << " global variables\n";
    }
    errs() << "\n";


    errs() << "Phase 3: Adding SanitizeThread attribute to affected functions, if not present\n";
    for (SharedResource V : SharedResources) {
        if (Instruction* I = dyn_cast<Instruction>(V.V)) {
            Function* F = I->getParent()->getParent();
            if (!F) continue;
            if (!F->hasFnAttribute(Attribute::SanitizeThread) && !F->hasFnAttribute(Attribute::DisableSanitizerInstrumentation)) {
                errs() << "Added Function '" << F->getName() << "' to be instrumented\n";
                F->addFnAttr(Attribute::SanitizeThread);
            }
        }
    }
    errs() << "\n";

    int type_counters[4] = {};
    for (SharedResource ShRes : SharedResources) {
        if (ShRes.Type == ShResType::RemoteBuf) type_counters[0]++;
        if (ShRes.Type == ShResType::ReadBuf) type_counters[1]++;
        if (ShRes.Type == ShResType::WriteBuf) type_counters[2]++;
        if (ShRes.Type == ShResType::DirtyBuf) type_counters[3]++;
    }
    errs() << "Found " << SharedResources.size() << " shared resources in total.\n";
    errs() << "----------------------------------------\n";
    errs() << type_counters[0] << " | RemoteBuf\n";
    errs() << type_counters[1] << " | ReadBuf\n";
    errs() << type_counters[2] << " | WriteBuf\n";
    errs() << type_counters[3] << " | DirtyBuf\n";
    errs() << "----------------------------------------\n";
    errs() << "Remote access types: ";
    errs() << (remoteisreadonly ? "RO " : "") << (remoteiswriteonly ? "WO " : "") << ((!remoteisreadonly && !remoteiswriteonly) ? "Mixed" : "") <<"\n";
    errs() << "\n";

    errs() << "Shared Resource detection finished, generating Result Struct" << "\n";
    createResultStruct(remoteisreadonly, remoteiswriteonly);
    errs() << "TSanRMAOpt done!" << "\n";
    return ResultVal;
}

int TSanRMAOptimizerAnalysis::recurseGenerateAllowlist(SharedResource V, RecurseConfig C, AAResults& AA) {
    int count = 0;
    std::vector<RecurseValue> WorkingSet;
    RecurseValue Init = {C,V};
    addUnique(WorkingSet, Init);
    std::vector<RecurseValue> NewSet;
    while (!WorkingSet.empty()) {
        for (RecurseValue CurRV : WorkingSet) {
            addShRes(CurRV);
            if (!CurRV.V.V->getType()->isPointerTy()) continue;
            if (CurRV.config.depth <= 1 && !CurRV.config.ignore_depth) continue;

            if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(CurRV.V.V)) {
                // ShRes is element of larger buffer. Mark that one as well.
                RecurseValue NewVal = create_recval(CurRV, GEP->getPointerOperand(), -1);
                addUnique(NewSet, NewVal);
            }
            if (LoadInst* Load = dyn_cast<LoadInst>(CurRV.V.V)) {
                // ShRes is element of larger buffer. Mark that one as well.
                if (Load->getType()->isPointerTy() && Load->getPointerOperandType()->isPointerTy()) {
                    RecurseValue NewVal = create_recval(CurRV, Load->getPointerOperand(), -1);
                    addUnique(NewSet, NewVal);
                }
            }
            // if (BitCastInst* BCast = dyn_cast<BitCastInst>(CurRV.V.V)) {
            //     // ShRes is being casted. If source is a pointer, instrument as well
            //     if (BCast->getSrcTy()->isPointerTy()) {
            //         RecurseValue NewVal = create_recval(CurRV, BCast->getOperand(1), -1);
            //         addUnique(NewSet, NewVal);
            //     }
            // }

            const Function* CurFunc = getParent(CurRV.V.V);
            if (CurFunc) {
                // Instrument parameters of function if used elsewhere
                for (const Argument& arg : CurFunc->args()) {
                    if (&arg == CurRV.V.V) {
                        int arg_num = arg.getArgNo();
                        // Current Value is a parameter in the function. Need to mark that parameter and instrument it on every call of that function
                        for (const Use& U : CurFunc->uses()) {
                            errs() << "Upward Func instr: " << CurFunc->getName() << "\n";
                            if (CallBase* FuncCall = dyn_cast<CallBase>(U.getUser())) {
                                std::optional<std::string> FuncName = getFuncName(FuncCall);
                                if (FuncName && *FuncName == "__kmpc_fork_call") {
                                    // Trying to escape upward from OpenMP outlined function. Needs special handling to correct arg numbers
                                    // __kmpc_fork_call takes additional parameter before shared vars begin
                                    arg_num++;
                                }
                                if (FuncName && *FuncName == CurFunc->getName()) {
                                    // Normal case, where CallBase corresponds directly to our current function
                                    RecurseValue NewVal = create_recval(CurRV, FuncCall->getArgOperand(arg_num), -1);
                                    addUnique(NewSet, NewVal);
                                }
                            }
                        }
                    }
                }
            }

            for (Use& U : CurRV.V.V->uses()) {
                if (CallBase* FuncCall = dyn_cast<CallBase>(U.getUser())) { // FuncCall: Enter Function, mark ShRes Argument and recurse
                    Function* F = FuncCall->getCalledFunction();
                    int operand_num = U.getOperandNo();
                    if (!F || F->isDeclaration()) {
                        // Definition not available. Check if it is some OpenMP call wrapper before bailing
                        std::optional<std::string> FuncNameOpt = getFuncName(FuncCall);
                        if (!FuncNameOpt || *FuncNameOpt != "__kmpc_fork_call") {
                            // Nope. Likely funcpointer or other library call
                            continue;
                        }
                        // Yep. Need to mark function inside this one
                        Function* OutlineFuncArg = dyn_cast<Function>(FuncCall->getArgOperand(2));
                        if (!OutlineFuncArg || OutlineFuncArg->isDeclaration()) {
                            errs() << "Unexpected type for OpenMP call wrapper argument! Hybrid support broken!\n";
                            continue;
                        }
                        errs() << "Detected OpenMP outlined function: " << OutlineFuncArg->getName() << "\n";
                        F = OutlineFuncArg;
                        operand_num--; // __kmpc_fork_call has 3 args before shared vars, but outlined function only 2
                    }
                    if (operand_num < F->arg_size()) {
                        Argument* ShArg = F->getArg(operand_num);
                        RecurseValue NewVal = create_recval(CurRV, ShArg, -1);
                        addUnique(NewSet, NewVal);
                    }
                } else if (LoadInst* Load = dyn_cast<LoadInst>(U.getUser())) { // LoadInst: ShRes might be loaded into other Pointer. Mark and recurse
                    RecurseValue NewVal = create_recval(CurRV, Load, -1);
                    addUnique(NewSet, NewVal);
                } else if (StoreInst* Store = dyn_cast<StoreInst>(U.getUser())) { // StoreInst: ShRes Pointer Val might be copied. Mark and recurse
                    RecurseValue NewVal = create_recval(CurRV, Store->getPointerOperand(), -1);
                    addUnique(NewSet, NewVal);
                    NewVal = create_recval(CurRV, Store->getValueOperand(), -1);
                    addUnique(NewSet, NewVal);
                } else if (ReturnInst* RInst = dyn_cast<ReturnInst>(U.getUser())){
                    // Instrument function return value as needed
                    if (!CurFunc) continue;
                    for (const Use& U : CurFunc->uses()) {
                        if (CallBase* FuncCall = dyn_cast<CallBase>(U.getUser())) {
                            RecurseValue NewVal = create_recval(CurRV, FuncCall, -1);
                            addUnique(NewSet, NewVal);
                        }
                    }
                } else {
                    if (!U.getUser()) continue;
                    for (SharedResource ShRes : SharedResources) { // Finally, check if some Value may be aliased to some ShRes, and, if it is, Mark and recurse
                        if (notDifferentParent(ShRes.V, U.getUser()) && AA.alias(U.getUser(),ShRes.V) != AliasResult::NoAlias) {
                            RecurseValue NewVal = create_recval(CurRV, U.getUser(), -1);
                            addUnique(NewSet, NewVal);
                            break;
                        }
                    }
                }
            }
        }
        count += WorkingSet.size();
        WorkingSet.clear();
        WorkingSet = NewSet;
        NewSet.clear();
    }
    return count;
}

void TSanRMAOptimizerAnalysis::createResultStruct(bool remoteisreadonly, bool remoteiswriteonly) {
    ResultVal.SharedResources = SharedResources;
    ResultVal.RemoteIsReadOnly = remoteisreadonly;
    ResultVal.RemoteIsWriteOnly = remoteiswriteonly;
}

bool TSanRMAOptimizerAnalysis::addUnique(std::vector<RecurseValue> &Set, RecurseValue V) {
    if (!V.V.V) return false;
    for (int i = 0; i < Set.size(); i++) {
        if (Set[i].V == V.V) {
            if (Set[i].V.Type != V.V.Type) {
                Set[i].V.Type = ShResType::DirtyBuf;
                V.V.Type = ShResType::DirtyBuf;
            }
            return false;
        }
    }

    // If this SharedResource has already been added, then skip if
    // (1) we ignore the depth
    // (2) the SharedResource has been added previously with the *same* type *and* with a larger or equal depth (no need to iterate over it again)
    auto res = std::find(SharedResources.begin(), SharedResources.end(), V.V);
    if (res != SharedResources.end() && (V.config.ignore_depth || (res->Type == V.V.Type && res->max_rem_depth >= V.config.depth))) {
        return false; // Value already considered in previous iteration, and added as a shared resource.
    }
    Set.push_back(V);
    return true;
}

bool TSanRMAOptimizerAnalysis::addShRes(RecurseValue& RV) {
    if (!RV.V.V) return false;
    if (!RV.config.allowGenerating) return false;
    for (int i = 0; i < SharedResources.size(); i++) {
        if (SharedResources[i].V == RV.V.V) {
            if (SharedResources[i].Type != RV.V.Type) {
                SharedResources[i].Type = ShResType::DirtyBuf;
                RV.V.Type = ShResType::DirtyBuf;
            }
            SharedResources[i].max_rem_depth = std::max(SharedResources[i].max_rem_depth, RV.config.depth);

            return false;
        }
    }
    RV.V.max_rem_depth = RV.config.depth;
    SharedResources.push_back(RV.V);
    return true;
}

TSanRMAOptimizerAnalysis::RecurseValue TSanRMAOptimizerAnalysis::create_recval(RecurseValue OrigRV, Value* val, int depthInc) {
    RecurseValue RV;
    RV.config = OrigRV.config;
    if (!OrigRV.config.ignore_depth)
        RV.config.depth += depthInc;
    RV.V.Type = OrigRV.V.Type;
    RV.V.V = val;
    return RV;
}

#undef DefaultConfig
