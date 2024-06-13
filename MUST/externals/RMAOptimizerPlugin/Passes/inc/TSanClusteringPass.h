#pragma once

#include "llvm/IR/PassManager.h"
#include <llvm/IR/InstrTypes.h>
#include <map>
#include <vector>

namespace llvm {

class TSanClusteringPass : public PassInfoMixin<TSanClusteringPass> {
    public:
        PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
    
    private:

        enum struct TSanCallType {
            TSanReadInst, TSanWriteInst
        };

        // Regex that matches TSan annotations, mapped to tuple of type and access size. Currently: Assume all buffer info is in first parameter
        const std::map<std::string,std::pair<TSanCallType,int>> TSanInstCalls = {
            {"__tsan_read1", {TSanCallType::TSanReadInst,1}},
            {"__tsan_read2", {TSanCallType::TSanReadInst,2}},
            {"__tsan_read4", {TSanCallType::TSanReadInst,4}},
            {"__tsan_read8", {TSanCallType::TSanReadInst,8}},
            {"__tsan_read16", {TSanCallType::TSanReadInst,16}},
            {"__tsan_write1", {TSanCallType::TSanWriteInst,1}},
            {"__tsan_write2", {TSanCallType::TSanWriteInst,2}},
            {"__tsan_write4", {TSanCallType::TSanWriteInst,4}},
            {"__tsan_write8", {TSanCallType::TSanWriteInst,8}},
            {"__tsan_write16", {TSanCallType::TSanWriteInst,16}},
        };

        struct TSanCall {
            CallBase* callValue;
            TSanCallType type;
            int accessSize;
        };

        void FlushClusters(std::list<std::vector<TSanCall>>& TSanClusters);
        int determineMaxAccessSize(std::vector<TSanCall>);
        int reduceConsistentCluster(std::vector<TSanCall>,int);
};

} // namespace llvm
