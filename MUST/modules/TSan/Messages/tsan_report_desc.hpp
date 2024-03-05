/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MUST_TSAN_REPORT_DESC_HPP
#define MUST_TSAN_REPORT_DESC_HPP

#include <cstdint>
#include "mustConfig.h"

namespace __tsan
{

enum TSanVersion { pre14, post14 };

/* simple structs that declare the report structure */

/* declarations from lib/sanitizer-common/sanitizer_internal_defs.h */
using uptr = std::uint64_t;
using u8 = unsigned char;

using Tid = unsigned int;
using tid_t = unsigned long long;

/* declarations from lib/sanitizer/sanitizer_common.h */
enum ModuleArch {
    kModuleArchUnknown,
    kModuleArchI386,
    kModuleArchX86_64,
    kModuleArchX86_64H,
    kModuleArchARMV6,
    kModuleArchARMV7,
    kModuleArchARMV7S,
    kModuleArchARMV7K,
    kModuleArchARM64,
    kModuleArchRISCV64,
    kModuleArchHexagon
};
const uptr kModuleUUIDSize = 32;

/* declarations from lib/sanitizer-common/sanitizer_vector.h */
template <typename T>
struct Vector {
    /* These are actually private */
    T* begin_;
    T* end_;
    T* last_;
};

/* declarations from lib/sanitizer-common/sanitizer_thread_registry.h */
enum class ThreadType {
    Regular, // Normal thread
    Worker,  // macOS Grand Central Dispatch (GCD) worker thread
    Fiber,   // Fiber
};

/* declarations from lib/sanitizer-common/sanitizer_symbolizer.h */

// For now, DataInfo is used to describe global variable.
struct DataInfo {
    // Owns all the string members. Storage for them is
    // (de)allocated using sanitizer internal allocator.
    char* module;
    uptr module_offset;
    ModuleArch module_arch;

    char* file;
    uptr line;
    char* name;
    uptr start;
    uptr size;
};

template <TSanVersion>
struct AddressInfo;

template <>
struct AddressInfo<TSanVersion::pre14> {
    // Owns all the string members. Storage for them is
    // (de)allocated using sanitizer internal allocator.
    uptr address;

    char* module;
    uptr module_offset;
    ModuleArch module_arch;

    static const uptr kUnknown;
    char* function;
    uptr function_offset;

    char* file;
    int line;
    int column;
};

template <>
struct AddressInfo<TSanVersion::post14> {
    // Owns all the string members. Storage for them is
    // (de)allocated using sanitizer internal allocator.
    uptr address;

    char* module;
    uptr module_offset;
    ModuleArch module_arch;
    // LLVM changed the AddressInfo layout in Version 14:
    // https://github.com/llvm/llvm-project/commit/edd2b99a57c127dc3d99fe7550d69a113de53eb0#diff-727f82578ab2c1e7bcaa5e33eb01fde1f8e17bf9ed5fa2c6144a7df966ea154b
    u8 uuid[kModuleUUIDSize];
    uptr uuid_size;

    static const uptr kUnknown;
    char* function;
    uptr function_offset;

    char* file;
    int line;
    int column;
};

// Linked list of symbolized frames (each frame is described by AddressInfo).
template <TSanVersion T>
struct SymbolizedStack {
    SymbolizedStack* next;
    AddressInfo<T> info;
};

/* declarations from lib/tsan/rtl/tsan_report.h */

enum ReportType {
    ReportTypeRace,
    ReportTypeVptrRace,
    ReportTypeUseAfterFree,
    ReportTypeVptrUseAfterFree,
    ReportTypeExternalRace,
    ReportTypeThreadLeak,
    ReportTypeMutexDestroyLocked,
    ReportTypeMutexDoubleLock,
    ReportTypeMutexInvalidAccess,
    ReportTypeMutexBadUnlock,
    ReportTypeMutexBadReadLock,
    ReportTypeMutexBadReadUnlock,
    ReportTypeSignalUnsafe,
    ReportTypeErrnoInSignal,
    ReportTypeDeadlock
};

template <TSanVersion T>
struct ReportStack {
    SymbolizedStack<T>* frames;
    bool suppressable;
};

struct ReportMopMutex {
    int id;
    bool write;
};

template <TSanVersion T>
struct ReportMop {
    int tid;
    uptr addr;
    int size;
    bool write;
    bool atomic;
    uptr external_tag;
    Vector<ReportMopMutex> mset;
    ReportStack<T>* stack;
};

enum ReportLocationType {
    ReportLocationGlobal,
    ReportLocationHeap,
    ReportLocationStack,
    ReportLocationTLS,
    ReportLocationFD
};

template <TSanVersion T>
struct ReportLocation {
    ReportLocationType type;
    DataInfo global;
    uptr heap_chunk_start;
    uptr heap_chunk_size;
    uptr external_tag;
    Tid tid;
    int fd;
    bool suppressable;
    ReportStack<T>* stack;
};

template <TSanVersion T>
struct ReportThread {
    Tid id;
    tid_t os_id;
    bool running;
    ThreadType thread_type;
    char* name;
    Tid parent_tid;
    ReportStack<T>* stack;
};

template <TSanVersion T>
struct ReportMutex {
    int id;
    uptr addr;
    ReportStack<T>* stack;
};

template <TSanVersion T>
class ReportDescT
{
  public:
    ReportType typ;
    uptr tag;
    Vector<ReportStack<T>*> stacks;
    Vector<ReportMop<T>*> mops;
    Vector<ReportLocation<T>*> locs;
    Vector<ReportMutex<T>*> mutexes;
    Vector<ReportThread<T>*> threads;
    Vector<Tid> unique_tids;
    ReportStack<T>* sleep;
    int count;
    int signum;
};
} // namespace __tsan

#endif // MUST_TSAN_REPORT_DESC_HPP
