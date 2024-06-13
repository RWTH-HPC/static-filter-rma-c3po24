#pragma once

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/DependenceAnalysis.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/PassManager.h"
#include <map>
#include <vector>
#include <set>

namespace llvm {

class TSanRMAOptimizerAnalysis : public AnalysisInfoMixin<TSanRMAOptimizerAnalysis> {
    public:
        static llvm::AnalysisKey Key;

        enum struct ShResType {
            RemoteBuf, ReadBuf, WriteBuf, DirtyBuf
        };

        struct SharedResource {
            Value* V;
            ShResType Type;
            // maximum remaining depth when SharedResource was discovered
            int max_rem_depth;
            bool operator==(const SharedResource ShRes) {
                return this->V == ShRes.V;
            }
        } typedef SharedResource;

        //Result Type
        struct Result {
            std::vector<SharedResource> SharedResources;
            bool RemoteIsReadOnly;
            bool RemoteIsWriteOnly;
            #warning HACK: Never invalidate TSanRMAOpt result
            bool invalidate(Module &M, const PreservedAnalyses &PA, ModuleAnalysisManager::Invalidator &) { return false; }
        } typedef TSanRMAOptResult;

        // Run Analysis
        Result run(Module &M, ModuleAnalysisManager &AM);

    private:
        // Result Store
        TSanRMAOptResult ResultVal;

        //Store var names containing shared resources
        std::vector<SharedResource> SharedResources;

        struct RecurseConfig {
            bool recurseDown; // Recurse over uses of current Value
            int depth; // Maximum depth for recursion
            bool ignore_depth; // Ignore above (unlimited search)
            bool allowGenerating; // Allow adding new values from this one
        };

        struct RecurseValue {
            RecurseConfig config;
            SharedResource V;
            bool operator==(const RecurseValue RV) {
                return this->V.V == RV.V.V;
            }
        };

        /* ---------------------------------------------- */
        /* START HERE TO ADD SUPPORT FOR OTHER FRAMEWORKS */

        #define LIST_OF_FRAMEWORKS \
            X(MPI_RMA) \
            X(OpenSHMEM) \
            X(GASPI)

        // enum is populated by above list, should not be modified
        enum struct RMAFramework {
            #define X(name) name,
            LIST_OF_FRAMEWORKS
            #undef X
        };

        const std::map<std::string,std::pair<std::set<int>,ShResType>> SharedResourceFactories = {
            // Format: (Function name regex, (Parameter indices with Shared Resources, Type created by Function)). If return type is ShRes: Param Index is -1

            // C Bindings - MPI RMA
            {"MPI_Win_create",        {{0}, ShResType::RemoteBuf}},
            {"MPI_Win_allocate",      {{4}, ShResType::RemoteBuf}},
            {"MPI_Win_get_attr",      {{2}, ShResType::RemoteBuf}},
            {"MPI_Put",               {{0}, ShResType::WriteBuf}},
            {"MPI_Rput",              {{0}, ShResType::WriteBuf}},
            {"MPI_Get",               {{0}, ShResType::ReadBuf}},
            {"MPI_Rget",              {{0}, ShResType::ReadBuf}},
            {"MPI_Accumulate",        {{0}, ShResType::WriteBuf}},
            {"MPI_Get_accumulate",    {{0,3}, ShResType::DirtyBuf}},
            {"MPI_Fetch_and_op",      {{0,1}, ShResType::DirtyBuf}},
            {"MPI_Compare_and_swap",  {{0,1,2}, ShResType::DirtyBuf}},

            // Fortran Bindings - MPI RMA
            {"mpi_win_create_",     {{0}, ShResType::RemoteBuf}},
            {"mpi_win_allocate_",   {{4}, ShResType::RemoteBuf}},
            {"mpi_put_",            {{0}, ShResType::WriteBuf}},
            {"mpi_rput_",           {{0}, ShResType::WriteBuf}},
            {"mpi_get_",            {{0}, ShResType::ReadBuf}},
            {"mpi_rget_",           {{0}, ShResType::ReadBuf}},
            {"mpi_accumulate_",     {{0}, ShResType::WriteBuf}},

            #warning TODO Test PGAS programming frameworks other than MPI RMA
            // C Bindings - OpenSHMEM
            {"shmem_malloc(_)?",                {{-1}, ShResType::RemoteBuf}},
            {"shmem_realloc(_)?",               {{-1}, ShResType::RemoteBuf}},
            {"shmem_align(_)?",                 {{-1}, ShResType::RemoteBuf}},
            {"shmem_malloc_with_hints(_)?",     {{-1}, ShResType::RemoteBuf}},
            {"shmem_calloc(_)?",                {{-1}, ShResType::RemoteBuf}},
            {"shmem_[a-zA-Z0-9]*_put(_nbi)?(_)?",  {{1}, ShResType::WriteBuf}},
            {"shmem_[a-zA-Z0-9]*_get(_nbi)?(_)?",  {{0}, ShResType::ReadBuf}},
            {"shmem_[a-zA-Z0-9]*_p(_nbi)?(_)?",  {{1}, ShResType::WriteBuf}},
            {"shmem_[a-zA-Z0-9]*_g(_nbi)?(_)?",  {{-1}, ShResType::ReadBuf}},
            {"shmem_putmem(_nbi)?(_)?",  {{1}, ShResType::WriteBuf}},
            {"shmem_getmem(_nbi)?(_)?",  {{0}, ShResType::ReadBuf}},
            {"shmem_[a-zA-Z0-9]*_atomic(_fetch)?(_(set|swap|compare_swap|inc|add|or|xor))?(_nbi)?(_)?",  {{-1}, ShResType::DirtyBuf}},
            // TODO: Think about adding shmem collectives

            // C Bindings - GASPI
            {"gaspi_segment_bind", {{1}, ShResType::RemoteBuf}},
            {"gaspi_segment_use", {{1}, ShResType::RemoteBuf}},
            {"gaspi_segment_ptr", {{1}, ShResType::RemoteBuf}}
        };

        // Map of (some) functions that, if at least one present, indicate that the mapped RMA framework is being used
        const std::map<std::string,RMAFramework> FrameworkInitializers = {
            {"MPI_Win_create",  RMAFramework::MPI_RMA},   // MPI RMA on C/C++ - Use Win create instead of MPI init to disambiguate two-sided MPI usage
            {"MPI_Win_allocate",  RMAFramework::MPI_RMA},   // MPI RMA on C/C++
            {"mpi_win_create_", RMAFramework::MPI_RMA},   // MPI RMA on Fortran
            {"mpi_win_allocate_", RMAFramework::MPI_RMA},   // MPI RMA on Fortran
            #warning TODO Test PGAS programming frameworks other than MPI RMA
            {"start_pes",      RMAFramework::OpenSHMEM}, // OpenSHMEM on C/C++
            {"shmem_init",      RMAFramework::OpenSHMEM}, // OpenSHMEM on C/C++
            {"shmem_init_thread", RMAFramework::OpenSHMEM}, // OpenSHMEM on C/C++
            {"start_pes_",      RMAFramework::OpenSHMEM}, // OpenSHMEM on Fortran
            {"shmem_init_",      RMAFramework::OpenSHMEM}, // OpenSHMEM on Fortran
            {"shmem_init_thread_", RMAFramework::OpenSHMEM}, // OpenSHMEM on Fortran
            {"gaspi_proc_init",   RMAFramework::GASPI},     // GASPI on C/C++
        };

        /* ---------------------------------------------- */

        friend llvm::raw_fd_ostream& operator<<(llvm::raw_fd_ostream& lhs, const RMAFramework FW) {
            switch(FW) {
                #define X(name) case RMAFramework::name:   lhs << #name;   break;
                LIST_OF_FRAMEWORKS
                #undef X
            }
            return lhs;
        }

        bool addUnique(std::vector<RecurseValue> &Set, RecurseValue V);

        bool addShRes(RecurseValue& V);

        // Recursively adds connected Values to allowlist according to Recursion Configuration
        int recurseGenerateAllowlist(SharedResource V, RecurseConfig C, AAResults& AA);

        // Helper Function that populates result struct
        void createResultStruct(bool RemoteIsReadOnly, bool RemoteIsWriteOnly);

        // Helper Function that creates temporary structs for allowlist generation
        RecurseValue create_recval(RecurseValue OrigRV, Value* val, int depthInc);
};

} // namespace llvm
