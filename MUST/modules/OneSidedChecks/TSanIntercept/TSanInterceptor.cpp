/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TSanInterceptor.cpp
 *       @see MUST::TSanInterceptor.
 *
 *  @date 26.07.2023
 *  @author Felix Tomski
 *  @author Sem Klauke
 */

#include "TSanInterceptor.h"

#include <algorithm>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
#include <dlfcn.h>
#include <link.h>

#include "BaseIds.h"
#include "GtiMacros.h"
#include "GtiTypes.h"
#include "I_Module.h"
#include "MustEnums.h"

using namespace gti;
using namespace must;

mGET_INSTANCE_FUNCTION(TSanInterceptor)
mFREE_INSTANCE_FUNCTION(TSanInterceptor)
mPNMPI_REGISTRATIONPOINT_FUNCTION(TSanInterceptor)

static bool finalized = false;

namespace must
{
namespace
{
class tsanInterceptorInitGuard
{
    /**
     * Gives access to #notify so that tsanInterceptor may call it in its constructor.
     */
    friend TSanInterceptor::TSanInterceptor(const char* instanceName);
    static std::atomic<bool> instance_created;

    /**
     * Notify the CallbackGuard that an instance of tsanInterceptor was constructed.
     */
    static auto notify() noexcept -> void
    {
        instance_created.store(true, std::memory_order_release);
    }

  public:
    /**
     * @return True iff an instance of tsanInterceptor has been created.
     */
    static auto is_some_instance_created() noexcept -> bool
    {
        return instance_created.load(std::memory_order_acquire);
    }
};
std::atomic<bool> tsanInterceptorInitGuard::instance_created{false};
} // namespace
} // namespace must

//=============================
// Constructor
//=============================
TSanInterceptor::TSanInterceptor(const char* instanceName)
    : gti::ModuleBase<TSanInterceptor, I_TSanInterceptor>(instanceName)
{
    // create sub modules
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUBMODULES 5
    if (subModInstances.size() < NUM_SUBMODULES) {
        std::cerr << "Module has not enough sub modules, check its analysis specification! ("
                  << __FILE__ << "@" << __LINE__ << ")" << std::endl;
        assert(0);
    }
    if (subModInstances.size() > NUM_SUBMODULES) {
        for (std::vector<I_Module*>::size_type i = NUM_SUBMODULES; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myLogger = (I_CreateMessage*)subModInstances[0];
    myGenLId = (I_GenerateLocationId*)subModInstances[1];
    myPIdMod = (I_ParallelIdAnalysis*)subModInstances[2];
    myParallelInit = (I_InitParallelId*)subModInstances[3];
    myLocationInit = (I_InitLocationId*)subModInstances[4];

    // get handleNewLocation function
    getWrapperFunction("handleNewLocation", (GTI_Fct_t*)&myNewLocFunc);
    // getWrapperFunction("propagateTSanAccess", (GTI_Fct_t*)&myPropagateAccess);
    getWrapperFunction("propagateTSanAccessBulk", (GTI_Fct_t*)&myPropagateAccessBulk);

    tsanInterceptorInitGuard::notify();
}

//=============================
// Destructor
//=============================
TSanInterceptor::~TSanInterceptor()
{
    if (myLogger != nullptr) {
        destroySubModuleInstance((I_Module*)myLogger);
        myLogger = nullptr;
    }
    if (myGenLId != nullptr) {
        destroySubModuleInstance((I_Module*)myGenLId);
        myGenLId = nullptr;
    }
    if (myPIdMod != nullptr) {
        destroySubModuleInstance((I_Module*)myPIdMod);
        myPIdMod = nullptr;
    }
    if (myParallelInit != nullptr) {
        destroySubModuleInstance((I_Module*)myParallelInit);
        myParallelInit = nullptr;
    }
    if (myLocationInit != nullptr) {
        destroySubModuleInstance((I_Module*)myLocationInit);
        myLocationInit = nullptr;
    }
}

MustParallelId TSanInterceptor::getParallelId() const
{
    MustParallelId id;
    myParallelInit->init(&id);
    return id;
}

MustLocationId TSanInterceptor::getLocationId(const void* pc) const
{
    MustLocationId id = myGenLId->getNextLocationId();
    Dl_info info;
    if (dladdr(pc, &info) == 0)
        return 0;
    void *callptr, *baseptr;

    Dl_info info2;
    struct link_map* link_map;
    size_t vmaOffset{0};
    if (dladdr1((void*)pc, &info2, (void**)&link_map, RTLD_DL_LINKMAP) != 0)
        vmaOffset = link_map->l_addr;

    if (PNMPI_Service_GetFunctionAddress(&callptr) == PNMPI_SUCCESS &&
        PNMPI_Service_GetSelfBaseAddress(&baseptr) == PNMPI_SUCCESS)
        (*myNewLocFunc)(
            getParallelId(),
            id,
            "\0",
            1,
            info.dli_fbase,
            (void*)((uintptr_t)pc - vmaOffset),
            info.dli_fname,
            strlen(info.dli_fname) + 1,
            info.dli_fbase
#ifdef ENABLE_STACKTRACE
            ,
            0,
            0,
            0,
            nullptr,
            nullptr
#endif
        );
    return id;
}

GTI_ANALYSIS_RETURN TSanInterceptor::init() { return GTI_ANALYSIS_SUCCESS; }

GTI_ANALYSIS_RETURN TSanInterceptor::fini()
{
    finalized = true;
    return GTI_ANALYSIS_SUCCESS;
}

GTI_ANALYSIS_RETURN TSanInterceptor::access(void* pc, int8_t isRead, void* addr, int64_t count)
{
    MustAddressType endAddr = ((MustAddressType)addr) + (count - 1);
    if (auto access = currentAccesses.find(addr); access != currentAccesses.end()) {
        // there alread was an access to addr in this tick
        if (endAddr >= access->second.endAddr[isRead]) {
            access->second.endAddr[isRead] = endAddr;
            access->second.pc[isRead].emplace(pc);
        }
    } else {
        // first access to addr in this tick
        currentAccesses.emplace(addr, AccessesForAddress(isRead, endAddr, pc));
    }
    return GTI_ANALYSIS_SUCCESS;
}

GTI_ANALYSIS_RETURN TSanInterceptor::tick()
{
    MustParallelId pId = getParallelId();

    // send in bulk as array
    if (!myPropagateAccessBulk)
        return GTI_ANALYSIS_SUCCESS;

    /* Aggregate memory ranges: [a,b] + [b,c] -> [a,c]:
     * For simplicity aggregate read and writes access seperate */
    for (int8_t isRead = 0; isRead <= 1; ++isRead) {
        // loop through saved accesses in ascending order (sort by starting mem address)
        for (auto it = currentAccesses.begin(); it != currentAccesses.end();) {
            // since std::map is ordered ascending we only need to look ahead
            auto lookahead_it = std::next(it);
            // look ahead as long as current.end+1 >= lookahead.start
            while (lookahead_it != currentAccesses.end() &&
                   (MustAddressType)lookahead_it->first <= it->second.endAddr[isRead] + 1) {
                // does to lookahead increases the address interval ?
                if (lookahead_it->second.endAddr[isRead] >= it->second.endAddr[isRead]) {
                    // it does. resize current
                    it->second.endAddr[isRead] = lookahead_it->second.endAddr[isRead];
                    it->second.pc[isRead].merge(lookahead_it->second.pc[isRead]);
                    // and clear the lookahead access
                    lookahead_it->second.endAddr[isRead] = 0;
                }
                ++lookahead_it;
            }
            it = lookahead_it;
        }
    }

    // save into vectors for consecutive storage
    BulkAccesses reads{currentAccesses.size()};
    BulkAccesses writes{currentAccesses.size()};

    for (auto const& [addr, access] : currentAccesses) {
        if (access.endAddr[0] != 0) {
            // write has stronger semantic than read
            writes.addr.emplace_back(addr);
            writes.endAddr.emplace_back((void*)access.endAddr[0]);
            std::copy(access.pc[0].begin(), access.pc[0].end(), std::back_inserter(writes.pc));
            writes.pcNum.emplace_back(access.pc[0].size());
        }
        if (access.endAddr[1] > access.endAddr[0]) {
            // propergate read only if it convers bigger address range
            reads.addr.emplace_back(addr);
            reads.endAddr.emplace_back((void*)access.endAddr[1]);
            std::copy(access.pc[1].begin(), access.pc[1].end(), std::back_inserter(reads.pc));
            reads.pcNum.emplace_back(access.pc[1].size());
        }
    }

    // send
    (*myPropagateAccessBulk)(
        pId,
        &reads.pc[0],
        &reads.pcNum[0],
        &reads.addr[0],
        &reads.endAddr[0],
        reads.addr.size(),
        reads.pc.size(),
        &writes.pc[0],
        &writes.pcNum[0],
        &writes.addr[0],
        &writes.endAddr[0],
        writes.addr.size(),
        writes.pc.size());

    currentAccesses.clear();

    return GTI_ANALYSIS_SUCCESS;
}

static TSanInterceptor* tsanInterceptorMod{nullptr};
inline bool is_instance_avail()
{
    if (finalized || !tsanInterceptorInitGuard::is_some_instance_created())
        return false;

    if (!tsanInterceptorMod)
        tsanInterceptorMod = TSanInterceptor::getInstance("");

    return true;
}

extern "C" void tsanInterceptInit(void)
{
    if (!is_instance_avail())
        return;

    tsanInterceptorMod->init();
}

extern "C" void tsanInterceptAccess(void* pc, int8_t isRead, void* addr, int64_t count)
{
    if (!is_instance_avail())
        return;

    tsanInterceptorMod->access(pc, isRead, addr, count);
}
