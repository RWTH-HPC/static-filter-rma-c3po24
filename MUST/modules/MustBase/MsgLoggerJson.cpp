/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file
 */

#include "MsgLoggerJson.h"

#include "GtiMacros.h"
#include "MustDefines.h"
#include <chrono>
#include <iomanip>
#include <sys/stat.h>

using namespace gti;
using namespace must;

mGET_INSTANCE_FUNCTION(MsgLoggerJson)
mFREE_INSTANCE_FUNCTION(MsgLoggerJson)
mPNMPI_REGISTRATIONPOINT_FUNCTION(MsgLoggerJson)

namespace
{
// map to switch between enum and string
#define map_entry_macro(name) {name, #name},
const std::map<MustMessageIdNames, std::string> MustErrorId2String{
    {MUST_MESSAGE_NO_ERROR, "MUST_MESSAGE_NO_ERROR"},
    // Errors
    FOREACH_MUST_ERRORS(map_entry_macro)
    // Warnings
    FOREACH_MUST_WARNING(map_entry_macro)
    // Informations
    FOREACH_MUST_INFO(map_entry_macro){MUST_LAST_MESSAGE_ID_NAME, "MUST_LAST_MESSAGE_ID_NAME"}};

#undef map_entry_macro

/**
 * Escapes a JSON string.
 * @param str the unescaped string
 * @return the JSON-escaped string
 */
auto escape(const std::string& str) -> std::string
{
    std::ostringstream escaped_string;
    for (auto unescaped_char : str) {
        switch (unescaped_char) {
        case '"':
            escaped_string << "\\\"";
            break;
        case '\\':
            escaped_string << "\\\\";
            break;
        case '\b':
            escaped_string << "\\b";
            break;
        case '\f':
            escaped_string << "\\f";
            break;
        case '\n':
            escaped_string << "\\n";
            break;
        case '\r':
            escaped_string << "\\r";
            break;
        case '\t':
            escaped_string << "\\t";
            break;
        default:
            if ('\x00' <= unescaped_char && unescaped_char <= '\x1f') {
                escaped_string << "\\u" << std::hex << std::setw(4) << std::setfill('0')
                               << static_cast<int>(unescaped_char);
            } else {
                escaped_string << unescaped_char;
            }
        }
    }
    return escaped_string.str();
}

/**
 * Serializes a range to a json array.
 * @tparam InputIt must satisfy the requirements of an InputIterator
 * @param out the stream to print to
 * @param begin iterator to the first element to serialize
 * @param end iterator to the element following the last element to serialize
 */
template <typename InputIt>
auto printJson(std::ostream& out, InputIt begin, InputIt end) -> void;

/**
 * Serializes an integer to json.
 * @param out the stream to print to
 * @param from the number to serialize
 */
auto printJson(std::ostream& out, int from) -> void { out << from; }

/**
 * Serializes a MustStackLevelInfo object to json.
 * @param out the stream to print to
 * @param from the object to serialize
 */
auto printJson(std::ostream& out, MustStackLevelInfo const& from) -> void
{
    out << R"({
        "function": ")"
        << from.symName << R"(",
        "file": ")"
        << from.fileModule << R"(",
        "line": )"
        << (from.lineOffset != "" ? from.lineOffset : "null") << R"(
        })";
}

#ifdef ENABLE_STACKTRACE
/**
 * Serializes a stacktrace to json.
 * @param out the stream to print to
 * @param stack the stacktrace to serialize
 */
auto printJson(std::ostream& out, std::list<MustStackLevelInfo> const& stack) -> void
{
    printJson(out, stack.begin(), stack.end());
}

/**
 * Serializes a MustStackLevelInfo object to json. The passed stack can be optional. If stack is
 * nullptr then "None" is printed.
 *
 * @param out the stream to print to
 * @param stack pointer to the (optional) stacktrace to serialize
 */
auto printJson(std::ostream& out, std::list<MustStackLevelInfo> const* stack) -> void
{
    if (stack != nullptr) {
        printJson(out, *stack);
    } else {
        out << "null";
    }
}
#endif

/**
 * Serializes a JsonFile::Message::Reference object.
 * @param out the stream to print to
 * @param from the object to serialize
 */
auto printJson(std::ostream& out, JsonFile::Message::Reference const& from) -> void
{
    out << R"({
  "no": )"
        << from.id << R"(,
  "call": ")"
        << from.locationInfo.callName << R"(",
  "rank": )"
        << from.parallelInfo.rank << R"(,
  "threadid": )"
        << from.parallelInfo.threadid;

    out << R"(,
  "stacktrace": )";
#ifdef ENABLE_STACKTRACE
    printJson(out, from.locationInfo.stack);
#else
    out << "null";
#endif
    out << R"(
})";
}

/**
 * Serializes a JsonFile::Message object.
 * @param out the stream to print to
 * @param msg the object to serialize
 */
auto printJson(std::ostream& out, JsonFile::Message const& msg) -> void
{
    out << R"({
      "type": ")"
        << msg.type << R"(",
      "error_id": ")"
        << msg.msgIdStr << R"(",
      "text": ")"
        << escape(msg.text) << R"(",
      "from": )";
    out << R"({
        "call": ")"
        << msg.callName << R"(",
        "stacktrace": )";
#ifdef ENABLE_STACKTRACE
    printJson(out, msg.location);
#else
    out << "null";
#endif
    out << R"(,
        "ranks": )";
    printJson(out, msg.ranks.begin(), msg.ranks.end());
    out << R"(,
        "ranks_strided": )"
        << (msg.strided ? "true" : "false") << R"(
      })";
    out << R"(,
      "references": )";
    printJson(out, msg.references.begin(), msg.references.end());
    out << R"(
    })";
}

template <typename InputIt>
auto printJson(std::ostream& out, InputIt begin, InputIt end) -> void
{
    out << "[\n";
    bool insert_comma = false;
    std::for_each(begin, end, [&](typename InputIt::value_type const& elem) {
        if (insert_comma) {
            out << ",\n";
        }
        insert_comma = true;
        printJson(out, elem);
    });
    out << "\n]";
}

} // namespace

auto JsonFile::printHeader() -> void
{
    *this << R"({
  "messages": [)";
}

auto JsonFile::print(JsonFile::Message const& msg) -> void
{
    eraseTrailer();

    if (myIsNonFirstMessage) {
        *this << ",";
    }
    myIsNonFirstMessage = true;

    printJson(*this, msg);

    printTrailer();
    if (msg.type == MustMessageType::MustErrorMessage) {
        flush();
    }
}

void JsonFile::eraseTrailer()
{
    std::ofstream::off_type const toErase = sizeof(myTrailer) - 1;
    // Unfortunately C++ does not offer an interface to truncate an ofstream. So we overwrite
    // carefully with spaces instead.
    seekp(-toErase, cur);
    *this << std::string(toErase, ' ');
    seekp(-toErase, cur);
}

auto JsonFile::printTrailer() -> void { *this << myTrailer; }

MsgLoggerJson::MsgLoggerJson(const char* instanceName) : ModuleBase(instanceName), MsgLoggerBase()
{
    std::vector<I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // save sub modules
    myLIdModule = (I_LocationAnalysis*)subModInstances[0];
    myPIdModule = (I_ParallelIdAnalysis*)subModInstances[1];
    must_ensure_dir_exists(get_base_output_dir().c_str());
    openFile(0, "MUST_Output.json", strlen("MUST_Output.json"));
}

MsgLoggerJson::~MsgLoggerJson()
{
    if (myLIdModule)
        destroySubModuleInstance((I_Module*)myLIdModule);
    myLIdModule = NULL;

    if (myPIdModule)
        destroySubModuleInstance((I_Module*)myPIdModule);
    myPIdModule = NULL;
}

gti::GTI_ANALYSIS_RETURN MsgLoggerJson::log(
    int msgId,
    int hasLocation,
    uint64_t pId,
    uint64_t lId,
    size_t fileId,
    int msgType,
    char* text,
    int textLen,
    int numReferences,
    uint64_t* refPIds,
    uint64_t* refLIds)
{
    if (hasLocation == 0) {
        return logStrided(
            msgId,
            pId,
            lId,
            fileId,
            0,
            0,
            0,
            msgType,
            text,
            textLen,
            numReferences,
            refPIds,
            refLIds);
    }

    return logStrided(
        msgId,
        pId,
        lId,
        fileId,
        myPIdModule->getInfoForId(pId).rank,
        1,
        1,
        msgType,
        text,
        textLen,
        numReferences,
        refPIds,
        refLIds);
}

gti::GTI_ANALYSIS_RETURN MsgLoggerJson::logStrided(
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
    uint64_t* refLIds)
{
    rememberErrorcode(msgType);
    bool strided = false;
    auto ranks = std::vector<int>{};
    if (count == 1) {
        ranks.push_back(startRank);
    } else {
        if (stride == 1) {
            ranks.resize(count);
            std::iota(ranks.begin(), ranks.end(), startRank);
        } else {
            strided = true;
            for (int i = 0; i < count; ++i) {
                ranks.push_back(startRank + i * stride);
            }
        }
    }

    auto references = std::vector<JsonFile::Message::Reference>{};
    references.reserve(numReferences);
    int refcounter = 0;
    auto const toReferenceString = [&](uint64_t refPId,
                                       uint64_t refLId) -> JsonFile::Message::Reference {
        refcounter++;
        return {
            refcounter,
            myLIdModule->getInfoForId(refPId, refLId),
            myPIdModule->getInfoForId(refPId)};
    };
    std::transform(
        refPIds,
        refPIds + numReferences,
        refLIds,
        std::back_inserter(references),
        toReferenceString);

    auto xptr = sf::xlock_safe_ptr(fileHandles);
    xptr->at(fileId).print(
        {MustErrorId2String.at(static_cast<MustMessageIdNames>(msgId)),
         static_cast<MustMessageType>(msgType),
         text,
         myLIdModule->getInfoForId(pId, lId).callName,
#ifdef ENABLE_STACKTRACE
         count > 0 ? &myLIdModule->getInfoForId(pId, lId).stack : nullptr,
#else
         nullptr,
#endif
         ranks,
         strided,
         references});

    return gti::GTI_ANALYSIS_SUCCESS;
}

auto MsgLoggerJson::openFile(size_t fileId, const char* filename, size_t len) -> void
{
    auto xptr = sf::xlock_safe_ptr(fileHandles);
    xptr->emplace(
        std::piecewise_construct,
        std::forward_as_tuple(fileId),
        std::forward_as_tuple(std::string(filename, len)));
}

auto MsgLoggerJson::closeFile(size_t fileId) -> void
{
    auto xptr = sf::xlock_safe_ptr(fileHandles);
    xptr->erase(fileId);
}
