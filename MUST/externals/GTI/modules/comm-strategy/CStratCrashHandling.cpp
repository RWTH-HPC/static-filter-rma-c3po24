/* This file is part of GTI (Generic Tool Infrastructure)
 *
 * Copyright (C)
 *  2008-2019 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2013-2019 RWTH Aachen University, Federal Republic of Germany
 *
 * See the LICENSE file in the package base directory for details
 */

/**
 * @file CStratCrashHandling.cpp
 *  Extension to communication strategies to handle crashes of the application.
 *
 *  This Extensions provides handlers for MPI-Errors and signals. By the use of
 * this handlers the tool processes can finish all analyses before the
 * application gets stopped.
 *
 *
 * @author Joachim Protze
 * @date 20.03.2012
 *
 */

#include <signal.h>
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <pnmpimod.h>
#include <unistd.h>
#include <assert.h>
#include <gtiConfig.h>
#include <sys/types.h>
#include <unistd.h>
#include <execinfo.h>

#ifndef _EXTERN_C_
#ifdef __cplusplus
#define _EXTERN_C_ extern "C"
#else /* __cplusplus */
#define _EXTERN_C_
#endif /* __cplusplus */
#endif /* _EXTERN_C_ */

#define GTI_SPLIT_MODULE_NAME "split_processes"

/* Switch between MPI-1.2 and MPI-2 errorhandler */
#ifdef HAVE_MPI_COMM_CREATE_ERRHANDLER
#define GTI_COMM_CREATE_ERRHANDLER(f, e) XMPI_Comm_create_errhandler(f, e)
#else
#define GTI_COMM_CREATE_ERRHANDLER(f, e) XMPI_Errhandler_create(f, e)
#endif

#ifdef HAVE_MPI_COMM_SET_ERRHANDLER
#define GTI_COMM_SET_ERRHANDLER(f, e)                                                              \
    if (f != MPI_COMM_NULL && e != 0)                                                              \
    XMPI_Comm_set_errhandler(f, e)
#else
#define GTI_COMM_SET_ERRHANDLER(f, e)                                                              \
    if (f != MPI_COMM_NULL && e != 0)                                                              \
    XMPI_Errhandler_set(f, e)
#endif

#define GTI_SET_ERR_HANDLER(c) GTI_COMM_SET_ERRHANDLER(*c, gtiMpiCommErrorhandler)

#ifdef GTI_STRAT_RAISE_PANIC
/**
 * Declaration of the panic function that the communication strategy should implement.
 * Use the macro mCOMM_STRATEGY_UP_RAISE_PANIC from GtiMacros.h for a default
 * implementation.
 */
void strategyRaisePanic(void);
#endif

typedef void (*sighandler_t)(int);

static void set_signalhandlers(sighandler_t handler)
{
    signal(SIGSEGV, handler);
    signal(SIGINT, handler);
    signal(SIGHUP, handler);
    signal(SIGABRT, handler);
    signal(SIGTERM, handler);
    signal(SIGUSR2, handler);
    signal(SIGQUIT, handler);
    signal(SIGALRM, handler);
}

__attribute__((destructor)) static void disable_signalhandlers() { set_signalhandlers(SIG_DFL); }

#define CALLSTACK_SIZE 20

static void print_stack(void)
{
    int nptrs;
    void* buf[CALLSTACK_SIZE + 1];

    nptrs = backtrace(buf, CALLSTACK_SIZE);

    backtrace_symbols_fd(buf, nptrs, STDOUT_FILENO);
}

MPI_Errhandler gtiMpiCommErrorhandler;
static int gtiMpiCrashRank, gtiMpiCrashSize, doStacktraceOnTerm;

void myMpiErrHandler(MPI_Comm* comm, int* errCode, ...)
{
    disable_signalhandlers();
    printf(
        "rank %i (of %i), pid %i caught MPI error nr %i\n",
        gtiMpiCrashRank,
        gtiMpiCrashSize,
        getpid(),
        *errCode);
    char error_string[BUFSIZ];
    int length_of_error_string;
    XMPI_Error_string(*errCode, error_string, &length_of_error_string);
    printf("%s\n", error_string);
    print_stack();

#ifdef GTI_STRAT_RAISE_PANIC
    strategyRaisePanic();
#endif
    printf("Waiting up to %i seconds for analyses to be finished.\n", GTI_CRASH_SLEEP_TIME);
    sleep(GTI_CRASH_SLEEP_TIME);
    exit(1);
}

void mySignalHandler(int signum)
{
    disable_signalhandlers();
    printf(
        "rank %i (of %i), pid %i caught signal nr %i\n",
        gtiMpiCrashRank,
        gtiMpiCrashSize,
        getpid(),
        signum);
    if (signum == SIGINT || signum == SIGKILL) {
        print_stack();
        MPI_Abort(MPI_COMM_WORLD, signum + 128);
    }
    if (signum == SIGTERM || signum == SIGUSR2) {
        if (doStacktraceOnTerm) {
            print_stack();
            fflush(stdout);
            sleep(1);
        }
        MPI_Abort(MPI_COMM_WORLD, signum + 128);
    }
    print_stack();
#ifdef GTI_STRAT_RAISE_PANIC
    strategyRaisePanic();
#endif
    printf("Waiting up to %i seconds for analyses to be finished.\n", GTI_CRASH_SLEEP_TIME);
    sleep(GTI_CRASH_SLEEP_TIME);
    exit(1);
}

void crashHandlingInit()
{
    int err;
    MPI_Comm this_set_comm;
    GTI_COMM_CREATE_ERRHANDLER(myMpiErrHandler, &gtiMpiCommErrorhandler);
    /**
     * @note we do not use GTI_COMM_SET_ERRHANDLER since the "if" in there
     * causes a segfault with Xcode 7.0.1 on OSX. The if is not needed so
     * we simply avoid it.
     */
    // GTI_COMM_SET_ERRHANDLER(MPI_COMM_SELF, gtiMpiCommErrorhandler);
#ifdef HAVE_MPI_COMM_SET_ERRHANDLER
    XMPI_Comm_set_errhandler(MPI_COMM_SELF, gtiMpiCommErrorhandler);
#else
    XMPI_Errhandler_set(MPI_COMM_SELF, gtiMpiCommErrorhandler);
#endif

    // === (2) Query for services of the split module ===
    PNMPI_modHandle_t handle;
    PNMPI_Service_descriptor_t service;

    err = PNMPI_Service_GetModuleByName(GTI_SPLIT_MODULE_NAME, &handle);
    /**
     * @note we do not use GTI_COMM_SET_ERRHANDLER since the "if" in there
     * causes a segfault with Xcode 7.0.1 on OSX. The if is not needed so
     * we simply avoid it.
     */
    // GTI_COMM_SET_ERRHANDLER(MPI_COMM_WORLD, gtiMpiCommErrorhandler);
#ifdef HAVE_MPI_COMM_SET_ERRHANDLER
    XMPI_Comm_set_errhandler(MPI_COMM_WORLD, gtiMpiCommErrorhandler);
#else
    XMPI_Errhandler_set(MPI_COMM_WORLD, gtiMpiCommErrorhandler);
#endif
    if (err != PNMPI_SUCCESS) {
        XMPI_Comm_size(MPI_COMM_WORLD, &gtiMpiCrashSize);
        XMPI_Comm_rank(MPI_COMM_WORLD, &gtiMpiCrashRank);
    } else {
        err = PNMPI_Service_GetServiceByName(handle, "SplitMod_getMySetComm", "p", &service);
        assert(err == PNMPI_SUCCESS);
        ((int (*)(void*))service.fct)(&this_set_comm);
        GTI_COMM_SET_ERRHANDLER(this_set_comm, gtiMpiCommErrorhandler);
        XMPI_Comm_size(this_set_comm, &gtiMpiCrashSize);
        XMPI_Comm_rank(this_set_comm, &gtiMpiCrashRank);
    }

    doStacktraceOnTerm = 0;
    char* envVar = getenv("INTERNAL_GTI_STACKTRACE_ON_TERM");
    if (envVar != NULL) {
        doStacktraceOnTerm = 1;
    }
    set_signalhandlers(mySignalHandler);
}

_EXTERN_C_ int MPI_Init(int* pArgc, char*** pArgv)
{
    int ret;
    ret = XMPI_Init(pArgc, pArgv);

    crashHandlingInit();
    return ret;
}

_EXTERN_C_ int MPI_Init_thread(int* pArgc, char*** pArgv, int request, int* provided)
{
    int ret;
    ret = XMPI_Init_thread(pArgc, pArgv, request, provided);

    crashHandlingInit();

    return ret;
}

// wrap does not generate MPI_Comm_spawn* functions
#if 0
#ifdef HAVE_MPI_COMM_SPAWN

// MPI-2
_EXTERN_C_ int MPI_Comm_spawn(
#ifndef HAVE_MPI_NO_CONST_CORRECTNESS
    const
#endif /*HAVE_MPI_NO_CONST_CORRECTNESS*/
    char* command,
    char* argv[],
    int maxprocs,
    MPI_Info info,
    int root,
    MPI_Comm comm,
    MPI_Comm* intercomm,
    int array_of_errcodes[])
{
    int ret =
        XMPI_Comm_spawn(command, argv, maxprocs, info, root, comm, intercomm, array_of_errcodes);
    GTI_COMM_SET_ERRHANDLER(*intercomm, gtiMpiCommErrorhandler);
    return ret;
}

// MPI-2
_EXTERN_C_ int MPI_Comm_spawn_multiple(
    int count,
    char* array_of_commands[],
    char** array_of_argv[],
#ifndef HAVE_MPI_NO_CONST_CORRECTNESS
    const
#endif /*HAVE_MPI_NO_CONST_CORRECTNESS*/
    int array_of_maxprocs[],
#ifndef HAVE_MPI_NO_CONST_CORRECTNESS
    const
#endif /*HAVE_MPI_NO_CONST_CORRECTNESS*/
    MPI_Info array_of_info[],
    int root,
    MPI_Comm comm,
    MPI_Comm* intercomm,
    int array_of_errcodes[])
{
    int ret = XMPI_Comm_spawn_multiple(
        count,
        array_of_commands,
        array_of_argv,
        array_of_maxprocs,
        array_of_info,
        root,
        comm,
        intercomm,
        array_of_errcodes);
    GTI_COMM_SET_ERRHANDLER(*intercomm, gtiMpiCommErrorhandler);
    return ret;
}

#endif // HAVE_MPI_COMM_SPAWN
#endif

#include "CStratCrashHandling.wrap.cpp" // NOLINT(bugprone-suspicious-include)
