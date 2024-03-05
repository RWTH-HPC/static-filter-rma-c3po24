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
 * @file CProtMpiSplitModule.cpp generated from cprot_mpi_split_module.w
 *       Splits MPI processes into multiple sets of processes.
 *       Intention is to use one set(id:0) for the actual application
 *       and the remaining sets for tool processes (one set for
 *       each level of the tool).
 *       Further, a separate stack is used for each of the sets,
 *       to enable tools with distinct layouts. Also, the actual
 *       application calls are separated such that MPI_COMM_WORLD
 *       is replaced by a comm representing the application
 *       processes set.
 *       For all the tool process sets only an MPI_Init and an
 *       MPI_Finalize is called, afterwards an exit(0) is issued,
 *       so all tool startup should be done in MPI_Init and it should
 *       only return once a shutdown is desired.
 *
 * @author Tobias Hilbrich
 * @date 27.07.2009
 */

#include <assert.h>
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <iomanip>
#include <sys/time.h>
#include <sys/resource.h>
#include <vector>
#include <map>
#include <string>
#include <unistd.h>
#include <sstream>

#include <dlfcn.h>

#include <pnmpimod.h>
#include "GtiMacros.h"
#include "CProtMpiSplitModule.h"

/**
 * Global definitions and helpers.
 */
/*Name of split module @todo move to some common place, currently present twice*/
#define MUST_SPLIT_MODULE_NAME "split_processes"

struct process_set_t {
    int size;
    int start_rank;
    int in_set : 2;    ///< If current rank is part of this set.
    int mpi_place : 2; ///< If this set consists of MPI places.
    int app_place : 2; ///< If this set consists of application places.
    MPI_Comm set_comm;
    int set_index;
    PNMPI_modHandle_t stack;
};

static std::vector<process_set_t> g_sets;
static std::map<std::pair<int, int>, int>
    g_mappings; /**< Maps a (OwnSetIndex,CommID) pair to a SetIndex, usage is to match equal
                   comm_ids to the right set indices.*/

/**
 * The below global variable is used for the MUST-DDT integration.
 * It has the following meaning:
 * # -1 => this is a tool process
 * # 0-N => this is application rank i
 */
static int MUST_application_rank = -1;

static bool gPrintMemoryConsumption = 0;

/**
 * This function serves for debugger integrations:
 * MUST issues it when its own initialization is finished to tell a potentially attached debugger
 * that the MUST initialization as well as MPI_Init was performed.
 */
extern "C" __attribute__((noinline)) void MUST_InitComplete(void)
{
    asm("");
#ifdef MUST_DEBUG
    std::cout << "MUST: " << getpid() << " has completed initialization." << std::endl;
#endif
}

/**
 * Query functions.
 */
extern "C" int getMySetSize(int* size)
{
    *size = -1;

    for (const auto& set : g_sets) {
        if (set.in_set == 1)
            *size = set.size;
    }
    return PNMPI_SUCCESS;
}

static MPI_Comm realCommWorld;
extern "C" int getRealCommWorld(void* comm)
{
    *((MPI_Comm*)comm) = realCommWorld;
    return PNMPI_SUCCESS;
}

extern "C" int getMySetComm(void* comm)
{
    *((MPI_Comm*)comm) = MPI_COMM_NULL;

    for (const auto& set : g_sets) {
        if (set.in_set == 1)
            *((MPI_Comm*)comm) = set.set_comm;
    }
    return PNMPI_SUCCESS;
}

extern "C" int getSetInfo(int commId, int* set_size, int* start_rank)
{
    int ownSetId = 0;
    unsigned setIdToUse;

    // get own set id
    for (std::size_t i = 0; i < g_sets.size(); i++) {
        if (g_sets[i].in_set == 1)
            ownSetId = i;
    }

    // find set id to use
    std::map<std::pair<int, int>, int>::iterator iter =
        g_mappings.find(std::make_pair(ownSetId, commId));

    if (iter != g_mappings.end())
        setIdToUse = iter->second; // New version use the mapping (if given)
    else
        setIdToUse = commId; // Old version gicen commId is the setId to use

    assert(setIdToUse < g_sets.size() && setIdToUse >= 0);

    *set_size = g_sets[setIdToUse].size;
    *start_rank = g_sets[setIdToUse].start_rank;

    return PNMPI_SUCCESS;
}
/**
 * PNMPI_ReistrationPoint
 */
extern "C" void PNMPI_RegistrationPoint()
{
    /* register this module*/
#if !defined(NDEBUG)
    int err =
#endif
        PNMPI_Service_RegisterModule(MUST_SPLIT_MODULE_NAME);
    assert(err == PNMPI_SUCCESS);
}

#ifdef MUST_TIME
static struct timeval gStart, gEnd;
#endif

void printSetMemoryConsumption(int gset_index)
{
    struct rusage r;
    getrusage(RUSAGE_SELF, &r);

    MPI_Comm setComm = g_sets[gset_index].set_comm;

    int commRank, commSize;
    PMPI_Comm_rank(setComm, &commRank);
    PMPI_Comm_size(setComm, &commSize);

    if (commRank != 0) {
        PMPI_Gather(&r.ru_maxrss, 1, MPI_LONG, nullptr, 1, MPI_LONG, 0, setComm);
    } else {
        // Collect and sum up data on a single rank for ordered output
        long procMemory[commSize];
        long totalMemory = 0;

        PMPI_Gather(&r.ru_maxrss, 1, MPI_LONG, procMemory, 1, MPI_LONG, 0, setComm);

        for (int j = 0; j < commSize; ++j) {
            totalMemory += procMemory[j];
        }

        std::stringstream memory_msg;
        memory_msg << std::setprecision(3) << std::fixed;
        memory_msg << "[GTI] Total Memory Consumption Layer " << gset_index
                   << " (max_rss): " << totalMemory / 1024.0 << " MiB" << std::endl;
        for (int j = 0; j < commSize; ++j) {
            memory_msg << "[GTI] Rank " << j << ", Layer " << gset_index << ":\t"
                       << procMemory[j] / 1024.0 << " MiB " << std::endl;
        }

        std::cout << memory_msg.str();
    }
}

extern "C" int MPI_Finalize()
{
    if (gPrintMemoryConsumption) {
        if (g_sets[0].app_place) {
            printSetMemoryConsumption(0);
        } else {
            // If we run MUST in hybrid mode, we output the result of
            // layer 1 here (which represents thread_app + thread_place layers).
            printSetMemoryConsumption(1);
        }
    }

    return XMPI_Finalize();
}

#include "CProtMpiSplitWorld.wrap.cpp" // NOLINT(bugprone-suspicious-include)
