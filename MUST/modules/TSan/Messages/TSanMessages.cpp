/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TSanMessages.cpp
 *       @see MUST::TSanMessages.
 *
 *  @date 23.11.2017
 *  @author Joachim Protze, Felix Dommes
 */

#include "TSanMessages.h"

#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "BaseIds.h"
#include "GtiMacros.h"
#include "GtiTypes.h"
#include "I_Module.h"
#include "MustEnums.h"
#include "PrefixedOstream.hpp"
#include "LocationInfo.h"

using namespace std;
using namespace gti;
using namespace must;

mGET_INSTANCE_FUNCTION(TSanMessages)
mFREE_INSTANCE_FUNCTION(TSanMessages)
mPNMPI_REGISTRATIONPOINT_FUNCTION(TSanMessages)

static bool finalized = false;

namespace
{

/**
 * Builds up a stack trace string from those given by the report.
 *
 * This constructs a stacktrace in a format that can be passed to the MUST
 * message mechanism. The format is built up as one big non-zero-terminated
 * string. It consists of the concatenated symbolname, filename and linenumber
 * for each stack level.
 * The `indices` point to the last character of each component in `locstrings`,
 * such that indices[i] is the offset in locStrings to the last character of
 * the i-th substring.
 *
 * @param stack the stack trace
 * @param locStrings[out] whole concatenated string
 * @param indices[out] the indices that mark the end of each substring of
 *                     locStrings
 * @param stack_depth[out] the stack strace's height
 */
template <__tsan::TSanVersion T>
void build_stacktrace(
    const __tsan::ReportStack<T>* const stack,
    std::string& locString,
    std::vector<int>& indices,
    int& stack_depth)
{
    stack_depth = 0;
    locString = "";
    indices.clear();
    for (auto* pFrame = stack->frames; pFrame != nullptr; pFrame = pFrame->next) {
        stack_depth += 1;
        // symbol name
        if (!pFrame->info.function)
            continue;
        locString.append(pFrame->info.function);
        indices.emplace_back(locString.length() - 1);
        // file/module name
        if (pFrame->info.file != nullptr) {
            locString.append(pFrame->info.file);
        } else if (pFrame->info.module != nullptr) {
            locString.append(pFrame->info.module);
        } else {
            continue;
        }
        indices.emplace_back(locString.length() - 1);
        // line/module offset number
        if (pFrame->info.line != 0) {
            locString.append(to_string(pFrame->info.line));
        } else {
            std::stringstream tmp{};
            tmp << "0x" << std::hex << pFrame->info.module_offset;
            locString.append(tmp.str());
        }
        indices.emplace_back(locString.length() - 1);
    }
}

} // namespace

namespace __tsan
{
/**
 * Called by OnReport() in OnReportLoader.cpp when Thread Sanitizer emits a
 * report.
 *
 * It forwards the report the the TSanMessages instance and prevents thread
 * sanitizer to print its own message to the console.
 */
extern "C" bool TsanOnReport(const __tsan::ReportDesc* rep, bool _suppressed, int llvm_version)
{
    auto* pre14Desc = (__tsan::ReportDescT<__tsan::TSanVersion::pre14>*)(rep);
    auto* post14Desc = (__tsan::ReportDescT<__tsan::TSanVersion::post14>*)(rep);
    // we only support data races
    switch (pre14Desc->typ) {
    case __tsan::ReportTypeRace:
        break;
    default:
        return _suppressed;
    }

    // forward data-race report
    if (!finalized) {
        TSanMessages* tsanMessages = TSanMessages::getInstance("");
        if (llvm_version < 14)
            tsanMessages->tsanReport(pre14Desc);
        else
            tsanMessages->tsanReport(post14Desc);
    }

    // returning true prevents tsan from printing its own report to stdout
    return _suppressed;
    // return false;
}
} /* namespace __tsan */

//=============================
// Constructor
//=============================
TSanMessages::TSanMessages(const char* instanceName)
    : gti::ModuleBase<TSanMessages, I_TSanMessages>(instanceName)
{
    // create sub modules
    vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // handle sub modules
#define NUM_SUBMODULES 3
    if (subModInstances.size() < NUM_SUBMODULES) {
        must::cerr << "Module has not enough sub modules, check its analysis specification! ("
                   << __FILE__ << "@" << __LINE__ << ")" << endl;
        assert(0);
    }
    if (subModInstances.size() > NUM_SUBMODULES) {
        for (vector<I_Module*>::size_type i = NUM_SUBMODULES; i < subModInstances.size(); i++)
            destroySubModuleInstance(subModInstances[i]);
    }

    myLogger = (I_CreateMessage*)subModInstances[0];
    myGenLId = (I_GenerateLocationId*)subModInstances[1];
    myPIdMod = (I_ParallelIdAnalysis*)subModInstances[2];

    // get handleNewLocation function
    getWrapperFunction("handleNewLocation", (GTI_Fct_t*)&myNewLocFunc);
}

//=============================
// Destructor
//=============================
TSanMessages::~TSanMessages()
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
}

/**
 * Formats a race message in MUST style from the Thread Sanitizer's report together with a list
 * list of references. The list of references is used to extract further information, e.g., the
 * remote rank in case of a remote data race.
 * @param report the report provided by Thread Sanitizer
 * @param refList list of (pid, lid) references to extract further information (e.g. remote ranks)
 * @return the formatted message string
 */
template <__tsan::TSanVersion T>
auto TSanMessages::format(
    const __tsan::ReportDescT<T>* const report,
    std::list<std::pair<MustParallelId, MustLocationId>>& refList) -> std::string
{
    std::stringstream msg;

    // extract list of ranks involved
    std::vector<int> rankList;
    std::transform(
        refList.begin(),
        refList.end(),
        std::back_inserter(rankList),
        [this](std::pair<MustParallelId, MustLocationId> ref) {
            return myPIdMod->getInfoForId(ref.first).rank;
        });

    // obtain own pId
    MustParallelId localpId = 0;
    getNodeInLayerId(&localpId);
    int localRank = myPIdMod->getInfoForId(localpId).rank;

    // If all ranks are the same: Local buffer race, otherwise remote data race
    bool isLocalRace = std::all_of(rankList.begin(), rankList.end(), [&](int rank) {
        return rank == rankList.front();
    });
    if (isLocalRace) {
        msg << "Local buffer data race at rank " << localRank << " between a ";
    } else {
        msg << "Remote data race at rank " << localRank << " between a ";
    }

    const auto* lastMop = (*report->mops.begin_);
    msg << (lastMop->write ? "write" : "read") << " of size " << lastMop->size << " at "
        << lastMop->stack->frames->info.function << "@1";
    if (!isLocalRace)
        msg << " from rank " << rankList.front();

    auto** it = report->mops.begin_ + 1;
    auto rank_it = rankList.begin() + 1;
    for (; it != report->mops.end_; ++it, ++rank_it) {
        auto* pMop = *it;
        if (it == report->mops.end_ - 1) {
            msg << " and ";
        } else {
            msg << ", ";
        }
        msg << "a previous " << (pMop->write ? "write" : "read") << " of size " << pMop->size
            << " at ";
        if (pMop->stack != nullptr)
            msg << pMop->stack->frames->info.function << "@"
                << distance(report->mops.begin_, it) + 1;
        else
            msg << "[failed to restore the stack]";
        if (!isLocalRace)
            msg << " from rank " << *rank_it;
    }
    msg << ".";
    return msg.str();
}

//=============================
// tsanReport
//=============================
template <__tsan::TSanVersion T>
auto TSanMessages::tsanReport(const __tsan::ReportDescT<T>* const report) -> GTI_ANALYSIS_RETURN
{
    auto lId = std::vector<MustParallelId>{};
#ifdef ENABLE_STACKTRACE
    auto locStrings = std::vector<std::string>{};
    auto indices = std::vector<std::vector<int>>{};
    auto stackLevels = std::vector<int>{};
#endif

    auto refList = list<pair<MustParallelId, MustLocationId>>{};

    // obtain IDs and register the locations
    MustParallelId pId = 0;
    // We can derive *our* pId by using the same approach as in InitParallelIdHybrid.cpp
    // without requiring InitParallelId to be called itself (otherwise the TSanMessages
    // module cannot run on tool thread layer).
    getNodeInLayerId(&pId);

    // extract locations
    auto locations = std::vector<LocationInfo>{};
    for (auto** it = report->mops.begin_; it != report->mops.end_; ++it) {
        auto* pMop = *it;
        auto stack_string = std::string{};
        auto stack_indices = std::vector<int>{};
        int levels = 0;
        auto stack = (*it)->stack;

        if (stack == nullptr)
            continue;

        // Check if we have a stack trace in the report that encodes (pId, lId) via delimiter
        // 0x0FFFFFFFFFFFFFFF. Such stack traces are of the format #0 - any frame #1 -
        // 0x0FFFFFFFFFFFFFFF #2 - pId #3 - lId
        if (stack->frames->next != nullptr &&
            stack->frames->next->info.address == 0x0FFFFFFFFFFFFFFF) {
            auto pId = stack->frames->next->next->info.address;
            auto lId = stack->frames->next->next->next->info.address;
            refList.push_back(std::make_pair(pId, lId));
        } else { // otherwise, do the usual processing by parsing the TSan stacktrace
            char* loc = pMop->stack->frames->info.function;
            if (loc != nullptr) {
                LocationInfo locInfo;
                locInfo.callName = loc;
                locInfo.codeptr = (void*)(pMop->stack->frames->info.module_offset + 1);
                locInfo.fname = pMop->stack->frames->info.module;
                locations.emplace_back(locInfo);
#ifdef ENABLE_STACKTRACE
                build_stacktrace((*it)->stack, stack_string, stack_indices, levels);
                locStrings.push_back(stack_string);
                indices.push_back(stack_indices);
                stackLevels.push_back(levels);
#endif
                MustLocationId location_id = myGenLId->getNextLocationId();
                lId.push_back(location_id);
                refList.emplace_back(make_pair(pId, location_id));
            }
        }
    }

    for (std::size_t i = 0; i < locations.size(); i++) {
        (*myNewLocFunc)(
            pId,
            lId[i],
            locations[i].callName.c_str(),
            locations[i].callName.length() + 1,
            NULL,
            locations[i].codeptr,
            locations[i].fname.c_str(),
            locations[i].fname.length() + 1,
            NULL
#ifdef ENABLE_STACKTRACE
            ,
            stackLevels[i],
            locStrings[i].length() + 1, // the length of all concatenated stack strings
            indices[i].size(),          // the number of indices (this is claimed to be convenient)
            indices[i].data(),
            locStrings[i].c_str() // should only be read access
#endif
        );
    }

    // format and create some nice message
    auto msg = format(report, refList);

    myLogger->createMessage(MUST_WARNING_DATARACE, pId, 0, MustErrorMessage, msg, refList);
    return GTI_ANALYSIS_SUCCESS;
}

GTI_ANALYSIS_RETURN TSanMessages::fini()
{
    finalized = true;
    return GTI_ANALYSIS_SUCCESS;
}
