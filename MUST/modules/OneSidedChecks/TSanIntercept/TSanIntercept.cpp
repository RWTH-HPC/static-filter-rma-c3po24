/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TSanIntercept.cpp
 *       @see TSanIntercept.
 *
 *  @date 26.07.2023
 *  @author Felix Tomski
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#define UNSET_GNU_SOURCE 1
#endif
#include <dlfcn.h>
#ifdef UNSET_GNU_SOURCE
#unset _GNU_SOURCE
#unset UNSET_GNU_SOURCE
#endif

#include <stdlib.h>
#include <stdio.h>
#include <cstring>

#if (defined __APPLE__) || (defined __MACH__)
void* handle = RTLD_DEFAULT;
#elif (defined __linux__) || (defined linux) || (defined __linux)
void* handle = RTLD_NEXT;
#else
void* handle = nullptr;
#endif

template <class FuncSig>
struct FuncInfo {
    bool tried{false};
    FuncSig f{nullptr};
    const char* name{};
    FuncInfo(const char* _name) : name(_name) {}
    template <class... Args>
    void exec(Args&&... args)
    {
        if (!tried) {
#ifdef MUST_DEBUG
            dlerror();
#endif
            f = (FuncSig)dlsym(handle, name);
            tried = true;
#ifdef MUST_DEBUG
            char* load_error = dlerror();
            if (load_error)
                fprintf(stderr, "Could not load %s symbol: %s\n", name, load_error);
            else if (f)
                fprintf(stderr, "Successfully loaded %s symbol.\n", name);
            else if (!f)
                fprintf(stderr, "Could not load %s symbol: dlsym returned nullptr.\n", name);
            dlerror();
#endif
        }
        if (!f)
            return;
#ifdef MUST_DEBUG
        printf("Calling func %s\n", name);
#endif
        (*f)(args...);
    }
};

#ifdef __cplusplus
extern "C" {
#endif

// Marker symbol to detect library
void __must_isl_tsan_interceptor(){};

typedef void (*InitFp)(void);
typedef void (*AccessFp)(void*, int8_t, void*, int64_t);

/* NOTE: Do not use static variables in function to avoid C++ ctor guard, which requires libstdc++.
 */
static FuncInfo<InitFp> initF("tsanInterceptInit");
void __tsan_init() { initF.exec(); }

void __tsan_acquire(void* addr) {}
void __tsan_release(void* addr) {}

typedef unsigned long uptr;
typedef signed long sptr;
typedef unsigned long long tid_t;

const char* __tsan_default_options();

void __tsan_flush_memory();

static FuncInfo<AccessFp> accessF("tsanInterceptAccess");
void __tsan_read1(void* addr) { accessF.exec(__builtin_return_address(0), 1, addr, 1); }
void __tsan_read2(void* addr) { accessF.exec(__builtin_return_address(0), 1, addr, 2); }
void __tsan_read4(void* addr) { accessF.exec(__builtin_return_address(0), 1, addr, 4); }
void __tsan_read8(void* addr) { accessF.exec(__builtin_return_address(0), 1, addr, 8); }
void __tsan_read16(void* addr) { accessF.exec(__builtin_return_address(0), 1, addr, 16); }

void __tsan_write1(void* addr) { accessF.exec(__builtin_return_address(0), 0, addr, 1); }
void __tsan_write2(void* addr) { accessF.exec(__builtin_return_address(0), 0, addr, 2); }
void __tsan_write4(void* addr) { accessF.exec(__builtin_return_address(0), 0, addr, 4); }
void __tsan_write8(void* addr) { accessF.exec(__builtin_return_address(0), 0, addr, 8); }
void __tsan_write16(void* addr) { accessF.exec(__builtin_return_address(0), 0, addr, 16); }

void __tsan_read1_pc(void* addr, void* pc) { accessF.exec(pc, 1, addr, 1); }
void __tsan_read2_pc(void* addr, void* pc) { accessF.exec(pc, 1, addr, 2); }
void __tsan_read4_pc(void* addr, void* pc) { accessF.exec(pc, 1, addr, 4); }
void __tsan_read8_pc(void* addr, void* pc) { accessF.exec(pc, 1, addr, 8); }
void __tsan_read16_pc(void* addr, void* pc) { accessF.exec(pc, 1, addr, 16); }

void __tsan_write1_pc(void* addr, void* pc) { accessF.exec(pc, 0, addr, 1); }
void __tsan_write2_pc(void* addr, void* pc) { accessF.exec(pc, 0, addr, 2); }
void __tsan_write4_pc(void* addr, void* pc) { accessF.exec(pc, 0, addr, 4); }
void __tsan_write8_pc(void* addr, void* pc) { accessF.exec(pc, 0, addr, 8); }
void __tsan_write16_pc(void* addr, void* pc) { accessF.exec(pc, 0, addr, 16); }

void __tsan_unaligned_read2(const void* addr) {}
void __tsan_unaligned_read4(const void* addr) {}
void __tsan_unaligned_read8(const void* addr) {}
void __tsan_unaligned_read16(const void* addr) {}

void __tsan_unaligned_write2(void* addr) {}
void __tsan_unaligned_write4(void* addr) {}
void __tsan_unaligned_write8(void* addr) {}
void __tsan_unaligned_write16(void* addr) {}

void __tsan_vptr_read(void** vptr_p) {}

void __tsan_vptr_update(void** vptr_p, void* new_val) {}

// https://github.com/llvm/llvm-project/blob/764287f1ad69469cc264bb094e8fcdcfdd0fcdfb/compiler-rt/lib/sanitizer_common/sanitizer_libc.cpp#L53
// needs #include <cstring>
void* __tsan_memcpy(void* dest, const void* src, uptr count) { return memcpy(dest, src, count); }

void* __tsan_memset(void* dest, int ch, uptr count) { return memset(dest, ch, count); }

void* __tsan_memmove(void* dest, const void* src, uptr count) { return memmove(dest, src, count); }

void __tsan_func_entry(void* call_pc) {}
void __tsan_func_exit() {}

void __tsan_ignore_thread_begin() {}
void __tsan_ignore_thread_end() {}

void __tsan_on_thread_idle() {}

void* __tsan_external_register_tag(const char* object_type);

void __tsan_external_register_header(void* tag, const char* header);

void __tsan_external_assign_tag(void* addr, void* tag);

void __tsan_external_read(void* addr, void* caller_pc, void* tag);

void __tsan_external_write(void* addr, void* caller_pc, void* tag);

void __tsan_read_range(void* addr, unsigned long size) {}

void __tsan_write_range(void* addr, unsigned long size) {}

void __tsan_read_range_pc(void* addr, unsigned long size, void* pc) {}

void __tsan_write_range_pc(void* addr, unsigned long size, void* pc) {}

// User may provide function that would be called right when TSan detects
// an error. The argument 'report' is an opaque pointer that can be used to
// gather additional information using other TSan report API functions.

void __tsan_on_report(void* report);

// If TSan is currently reporting a detected issue on the current thread,
// returns an opaque pointer to the current report. Otherwise returns NULL.

void* __tsan_get_current_report();

// Returns a report's description (issue type), number of duplicate issues
// found, counts of array data (stack traces, memory operations, locations,
// mutexes, threads, unique thread IDs) and a stack trace of a sleep() call (if
// one was involved in the issue).

int __tsan_get_report_data(
    void* report,
    const char** description,
    int* count,
    int* stack_count,
    int* mop_count,
    int* loc_count,
    int* mutex_count,
    int* thread_count,
    int* unique_tid_count,
    void** sleep_trace,
    uptr trace_size);

/// Retrieves the "tag" from a report (for external-race report types). External
/// races can be associated with a tag which give them more meaning. For example
/// tag value '1' means "Swift access race". Tag value '0' indicated a plain
/// external race.
///
/// \param report opaque pointer to the current report (obtained as argument in
///               __tsan_on_report, or from __tsan_get_current_report)
/// \param [out] tag points to storage that will be filled with the tag value
///
/// \returns non-zero value on success, zero on failure

int __tsan_get_report_tag(void* report, uptr* tag);

// Returns information about stack traces included in the report.

int __tsan_get_report_stack(void* report, uptr idx, void** trace, uptr trace_size);

// Returns information about memory operations included in the report.

int __tsan_get_report_mop(
    void* report,
    uptr idx,
    int* tid,
    void** addr,
    int* size,
    int* write,
    int* atomic,
    void** trace,
    uptr trace_size);

// Returns information about locations included in the report.

int __tsan_get_report_loc(
    void* report,
    uptr idx,
    const char** type,
    void** addr,
    uptr* start,
    uptr* size,
    int* tid,
    int* fd,
    int* suppressable,
    void** trace,
    uptr trace_size);

int __tsan_get_report_loc_object_type(void* report, uptr idx, const char** object_type);

// Returns information about mutexes included in the report.

int __tsan_get_report_mutex(
    void* report,
    uptr idx,
    uptr* mutex_id,
    void** addr,
    int* destroyed,
    void** trace,
    uptr trace_size);

// Returns information about threads included in the report.

int __tsan_get_report_thread(
    void* report,
    uptr idx,
    int* tid,
    tid_t* os_id,
    int* running,
    const char** name,
    int* parent_tid,
    void** trace,
    uptr trace_size);

// Returns information about unique thread IDs included in the report.

int __tsan_get_report_unique_tid(void* report, uptr idx, int* tid);

// Returns the type of the pointer (heap, stack, global, ...) and if possible
// also the starting address (e.g. of a heap allocation) and size.

const char* __tsan_locate_address(
    uptr addr,
    char* name,
    uptr name_size,
    uptr* region_address,
    uptr* region_size);

// Returns the allocation stack for a heap pointer.

int __tsan_get_alloc_stack(uptr addr, uptr* trace, uptr size, int* thread_id, tid_t* os_id);

// These should match declarations from public tsan_interface_atomic.h header.
typedef unsigned char a8;
typedef unsigned short a16;
typedef unsigned int a32;
typedef unsigned long long a64;
#if !SANITIZER_GO &&                                                                               \
    (defined(__SIZEOF_INT128__) || (__clang_major__ * 100 + __clang_minor__ >= 302)) &&            \
    !defined(__mips64) && !defined(__s390x__)
__extension__ typedef __int128 a128;
#define __TSAN_HAS_INT128 1
#else
#define __TSAN_HAS_INT128 0
#endif

// Part of ABI, do not change.
// https://github.com/llvm/llvm-project/blob/main/libcxx/include/atomic
typedef enum { mo_relaxed, mo_consume, mo_acquire, mo_release, mo_acq_rel, mo_seq_cst } morder;

struct ThreadState;

a8 __tsan_atomic8_load(const volatile a8* a, morder mo) { return 0; }

a16 __tsan_atomic16_load(const volatile a16* a, morder mo) { return 0; }

a32 __tsan_atomic32_load(const volatile a32* a, morder mo) { return 0; }

a64 __tsan_atomic64_load(const volatile a64* a, morder mo) { return 0; }
#if __TSAN_HAS_INT128

a128 __tsan_atomic128_load(const volatile a128* a, morder mo) { return 0; }
#endif

void __tsan_atomic8_store(volatile a8* a, a8 v, morder mo) {}

void __tsan_atomic16_store(volatile a16* a, a16 v, morder mo) {}

void __tsan_atomic32_store(volatile a32* a, a32 v, morder mo) {}

void __tsan_atomic64_store(volatile a64* a, a64 v, morder mo) {}
#if __TSAN_HAS_INT128

void __tsan_atomic128_store(volatile a128* a, a128 v, morder mo) {}
#endif

a8 __tsan_atomic8_exchange(volatile a8* a, a8 v, morder mo) { return 0; }

a16 __tsan_atomic16_exchange(volatile a16* a, a16 v, morder mo) { return 0; }

a32 __tsan_atomic32_exchange(volatile a32* a, a32 v, morder mo) { return 0; }

a64 __tsan_atomic64_exchange(volatile a64* a, a64 v, morder mo) { return 0; }
#if __TSAN_HAS_INT128

a128 __tsan_atomic128_exchange(volatile a128* a, a128 v, morder mo) { return 0; }
#endif

a8 __tsan_atomic8_fetch_add(volatile a8* a, a8 v, morder mo) { return 0; }

a16 __tsan_atomic16_fetch_add(volatile a16* a, a16 v, morder mo) { return 0; }

a32 __tsan_atomic32_fetch_add(volatile a32* a, a32 v, morder mo) { return 0; }

a64 __tsan_atomic64_fetch_add(volatile a64* a, a64 v, morder mo) { return 0; }
#if __TSAN_HAS_INT128

a128 __tsan_atomic128_fetch_add(volatile a128* a, a128 v, morder mo) { return 0; }
#endif

a8 __tsan_atomic8_fetch_sub(volatile a8* a, a8 v, morder mo) { return 0; }

a16 __tsan_atomic16_fetch_sub(volatile a16* a, a16 v, morder mo) { return 0; }

a32 __tsan_atomic32_fetch_sub(volatile a32* a, a32 v, morder mo) { return 0; }

a64 __tsan_atomic64_fetch_sub(volatile a64* a, a64 v, morder mo) { return 0; }
#if __TSAN_HAS_INT128

a128 __tsan_atomic128_fetch_sub(volatile a128* a, a128 v, morder mo) { return 0; }
#endif

a8 __tsan_atomic8_fetch_and(volatile a8* a, a8 v, morder mo) { return 0; }

a16 __tsan_atomic16_fetch_and(volatile a16* a, a16 v, morder mo) { return 0; }

a32 __tsan_atomic32_fetch_and(volatile a32* a, a32 v, morder mo) { return 0; }

a64 __tsan_atomic64_fetch_and(volatile a64* a, a64 v, morder mo) { return 0; }
#if __TSAN_HAS_INT128

a128 __tsan_atomic128_fetch_and(volatile a128* a, a128 v, morder mo) { return 0; }
#endif

a8 __tsan_atomic8_fetch_or(volatile a8* a, a8 v, morder mo) { return 0; }

a16 __tsan_atomic16_fetch_or(volatile a16* a, a16 v, morder mo) { return 0; }

a32 __tsan_atomic32_fetch_or(volatile a32* a, a32 v, morder mo) { return 0; }

a64 __tsan_atomic64_fetch_or(volatile a64* a, a64 v, morder mo) { return 0; }
#if __TSAN_HAS_INT128

a128 __tsan_atomic128_fetch_or(volatile a128* a, a128 v, morder mo) { return 0; }
#endif

a8 __tsan_atomic8_fetch_xor(volatile a8* a, a8 v, morder mo) { return 0; }

a16 __tsan_atomic16_fetch_xor(volatile a16* a, a16 v, morder mo) { return 0; }

a32 __tsan_atomic32_fetch_xor(volatile a32* a, a32 v, morder mo) { return 0; }

a64 __tsan_atomic64_fetch_xor(volatile a64* a, a64 v, morder mo) { return 0; }
#if __TSAN_HAS_INT128

a128 __tsan_atomic128_fetch_xor(volatile a128* a, a128 v, morder mo) { return 0; }
#endif

a8 __tsan_atomic8_fetch_nand(volatile a8* a, a8 v, morder mo) { return 0; }

a16 __tsan_atomic16_fetch_nand(volatile a16* a, a16 v, morder mo) { return 0; }

a32 __tsan_atomic32_fetch_nand(volatile a32* a, a32 v, morder mo) { return 0; }

a64 __tsan_atomic64_fetch_nand(volatile a64* a, a64 v, morder mo) { return 0; }
#if __TSAN_HAS_INT128

a128 __tsan_atomic128_fetch_nand(volatile a128* a, a128 v, morder mo) { return 0; }
#endif

int __tsan_atomic8_compare_exchange_strong(volatile a8* a, a8* c, a8 v, morder mo, morder fmo)
{
    return 0;
}

int __tsan_atomic16_compare_exchange_strong(volatile a16* a, a16* c, a16 v, morder mo, morder fmo)
{
    return 0;
}

int __tsan_atomic32_compare_exchange_strong(volatile a32* a, a32* c, a32 v, morder mo, morder fmo)
{
    return 0;
}

int __tsan_atomic64_compare_exchange_strong(volatile a64* a, a64* c, a64 v, morder mo, morder fmo)
{
    return 0;
}
#if __TSAN_HAS_INT128

int __tsan_atomic128_compare_exchange_strong(
    volatile a128* a,
    a128* c,
    a128 v,
    morder mo,
    morder fmo)
{
    return 0;
}
#endif

int __tsan_atomic8_compare_exchange_weak(volatile a8* a, a8* c, a8 v, morder mo, morder fmo)
{
    return 0;
}

int __tsan_atomic16_compare_exchange_weak(volatile a16* a, a16* c, a16 v, morder mo, morder fmo)
{
    return 0;
}

int __tsan_atomic32_compare_exchange_weak(volatile a32* a, a32* c, a32 v, morder mo, morder fmo)
{
    return 0;
}

int __tsan_atomic64_compare_exchange_weak(volatile a64* a, a64* c, a64 v, morder mo, morder fmo)
{
    return 0;
}
#if __TSAN_HAS_INT128

int __tsan_atomic128_compare_exchange_weak(volatile a128* a, a128* c, a128 v, morder mo, morder fmo)
{
    return 0;
}
#endif

a8 __tsan_atomic8_compare_exchange_val(volatile a8* a, a8 c, a8 v, morder mo, morder fmo)
{
    return 0;
}

a16 __tsan_atomic16_compare_exchange_val(volatile a16* a, a16 c, a16 v, morder mo, morder fmo)
{
    return 0;
}

a32 __tsan_atomic32_compare_exchange_val(volatile a32* a, a32 c, a32 v, morder mo, morder fmo)
{
    return 0;
}

a64 __tsan_atomic64_compare_exchange_val(volatile a64* a, a64 c, a64 v, morder mo, morder fmo)
{
    return 0;
}
#if __TSAN_HAS_INT128

a128 __tsan_atomic128_compare_exchange_val(volatile a128* a, a128 c, a128 v, morder mo, morder fmo)
{
    return 0;
}
#endif

void __tsan_atomic_thread_fence(morder mo) {}

void __tsan_atomic_signal_fence(morder mo) {}

#ifdef __cplusplus
}
#endif
