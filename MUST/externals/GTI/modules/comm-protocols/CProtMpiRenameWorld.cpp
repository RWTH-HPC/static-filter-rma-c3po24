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
#include <sys/time.h>
#include <vector>
#include <map>
#include <string>
#include <unistd.h>
#include <sstream>

#include <dlfcn.h>

#include <pnmpimod.h>
#include "GtiMacros.h"

/**
 * Global definitions and helpers.
 */
/*Name of split module @todo move to some common place, currently present twice*/
#define MUST_SPLIT_MODULE_NAME "replace_module"
MPI_Comm fakeComm;
#define MACRO_MPI_Comm(_c)                                                                         \
    {                                                                                              \
        if (_c == MPI_COMM_WORLD)                                                                  \
            _c = fakeComm;                                                                         \
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
struct timeval gStart, gEnd;
#endif

#include "CProtMpiRenameWorld.wrap.cpp" // NOLINT(bugprone-suspicious-include)
