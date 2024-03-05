#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#define UNSET_GNU_SOURCE 1
#endif
#include <dlfcn.h>
#ifdef UNSET_GNU_SOURCE
#unset _GNU_SOURCE
#unset UNSET_GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>

namespace __tsan
{
class ReportDesc;
typedef bool (*OnReportFp)(const ReportDesc*, bool, int);

const auto* env_disable_onreport = getenv("MUST_DISABLE_TSAN_ONREPORT");
const auto* env_enable_dlerror = getenv("MUST_ENABLE_TSAN_DLERROR");
bool OnReport(const ReportDesc* rep, bool suppressed)
{
    static OnReportFp f = nullptr;
    static bool tried = false;
    static bool disabled = false;

    if (!tried) {
        if (env_disable_onreport && atoi(env_disable_onreport)) {
            disabled = true;
            return suppressed;
        }
        void* handle = nullptr;
#if (defined __APPLE__) || (defined __MACH__)
        handle = RTLD_DEFAULT;
#elif (defined __linux__) || (defined linux) || (defined __linux)
        handle = RTLD_NEXT;
#else
        return suppressed;
#endif
        if (env_enable_dlerror && atoi(env_enable_dlerror))
            dlerror();
        f = (OnReportFp)dlsym(handle, "TsanOnReport");
#ifdef MUST_DEBUG
        char* load_error = dlerror();
        if (load_error)
            fprintf(stderr, "Could not load TsanOnReport symbol: %s", load_error);
        else if (f)
            fprintf(stderr, "Successfully loaded TsanOnReport symbol.\n");
        else if (!f)
            fprintf(stderr, "Could not load TsanOnReport symbol: dlsym returned nullptr.\n");
#else
        if (env_enable_dlerror && atoi(env_enable_dlerror))
            dlerror();
#endif
        tried = true;
    }

    if (!disabled && f)
#ifdef __clang_major__
        return (*f)(rep, suppressed, __clang_major__);
#else
        return (*f)(rep, suppressed, 13);
#endif

    return suppressed;
}
} /* namespace __tsan */
