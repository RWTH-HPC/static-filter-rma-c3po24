/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

//
// Created by sebastian on 15.12.22.
//

#ifndef MUST_MSGLOGGERJSON_H
#define MUST_MSGLOGGERJSON_H

#include "I_MessageLogger.h"
#include <fstream>
#include <mutex>

#include "ModuleBase.h"

#include "I_LocationAnalysis.h"
#include "I_ParallelIdAnalysis.h"
#include "MustDefines.h"
#include "MustEnums.h"
#include "NamedOfstream.hpp"
#include "MsgLoggerCommon.hpp"
#include "safe_ptr.h"

namespace must
{

class JsonFile : NamedOfstream
{
  private:
    static auto constexpr myTrailer = R"(
  ]
}
)";

    bool myIsNonFirstMessage{false};

    auto printHeader() -> void;

    auto printTrailer() -> void;

    auto eraseTrailer() -> void;

    auto printInitial() -> void
    {
        printHeader();
        printTrailer();
    };

  public:
    struct Message {
        struct Reference {
            int id;
            const LocationInfo& locationInfo;
            ParallelInfo parallelInfo;
        };

        const std::string& msgIdStr;
        MustMessageType type;
        const std::string& text;
        const std::string& callName;
        const std::list<MustStackLevelInfo>* location;
        const std::vector<int>& ranks;
        bool strided;
        const std::vector<Reference>& references;
    };

    JsonFile() { printInitial(); }

    JsonFile(const std::string& filename) : NamedOfstream(filename) { printInitial(); }

    auto print(const Message& msg) -> void;
};

/**
 * Implementation of I_MessageLogger that writes
 * a JSON file.
 */
class MsgLoggerJson : public gti::ModuleBase<MsgLoggerJson, I_MessageLogger, false>,
                      public MsgLoggerBase
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    MsgLoggerJson(const char* instanceName);

    /**
     * Destructor.
     */
    ~MsgLoggerJson() override;

    /**
     * @see I_MessageLogger::log.
     */
    gti::GTI_ANALYSIS_RETURN
    log(int msgId,
        int hasLocation,
        uint64_t pId,
        uint64_t lId,
        size_t fileId,
        int msgType,
        char* text,
        int textLen,
        int numReferences,
        uint64_t* refPIds,
        uint64_t* refLIds) override;

    /**
     * @see I_MessageLogger::logStrided.
     */
    gti::GTI_ANALYSIS_RETURN logStrided(
        int msgId,
        uint64_t pId,
        uint64_t lId,
        size_t fileId,
        int startRank,
        int stride,
        int count,
        int msgType,
        char* text,
        int textLen,
        int numReferences,
        uint64_t* refPIds,
        uint64_t* refLIds) override;

    /**
     * @see I_MessageLogger::openFile
     */
    auto openFile(size_t fileId, const char* filename, size_t len) -> void override;

    /**
     * @see I_MessageLogger::closeFile
     */
    auto closeFile(size_t fileId) -> void override;

  private:
    /**
     * A map of open file handles.
     *
     * This map maps the fileId of open log-files to its corresponding
     * file stream.
     */
    sf::contfree_safe_ptr<std::map<size_t, JsonFile>> fileHandles{};

}; /*class MsgLoggerJson */
} // namespace must

#endif // MUST_MSGLOGGERJSON_HPP
