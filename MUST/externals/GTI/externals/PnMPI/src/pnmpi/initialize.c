/* This file is part of P^nMPI.
 *
 * Copyright (c)
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2011-2016 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2013-2019 RWTH Aachen University, Federal Republic of Germany
 *
 *
 * P^nMPI is free software; you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation version 2.1 dated February 1999.
 *
 * P^nMPI is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with P^nMPI; if not, write to the
 *
 *   Free Software Foundation, Inc.
 *   51 Franklin St, Fifth Floor
 *   Boston, MA 02110, USA
 *
 *
 * Written by Martin Schulz, schulzm@llnl.gov.
 *
 * LLNL-CODE-402774
 */

#include "core.h"
#include <assert.h>
#include <pnmpi/private/attributes.h>
#include <pnmpi/private/config.h>
#include <pnmpi/private/initialization.h>

// Check for mpich 4.0 due to this bug:
// https://github.com/pmodels/mpich/issues/5805
#if defined(MPICH_NUMVERSION) && MPICH_NUMVERSION > 40000000 && \
  MPICH_NUMVERSION < 40100000
#define PNMPI_QUIRK_MPICH_SESSION_INIT_ORDER
#endif

/** \brief Global counter for number of initializations.
 *
 * \details This counter will be increased on any call to the initialization
 *  functions and decreased on finalization. It ensures PnMPI is initialized
 *  only once and finalized in the last finalization call.
 *
 *
 * \private
 */
PNMPI_INTERNAL
int pnmpi_initialized = 0;

#if defined(PNMPI_ENABLE_SESSIONS)
#if defined(PNMPI_QUIRK_MPICH_SESSION_INIT_ORDER)
/**
 * Create and immediately discard a communicator for the given session pset.
 *
 * This seemingly useless function is a helper for applying the workaround to an
 * initialization issue of mpich 4.0.x
 *
 * @param session a valid session handle to use
 * @param pset zero-terminated string with the url of the psocess set, e.g.
 *        mpi://WORLD
 * @internal
 */
PNMPI_UNUSED static void pnmpi_comm_create_discard(MPI_Session session,
                                                   const char *pset)
{
  assert(session != MPI_SESSION_NULL);

  MPI_Group group = MPI_GROUP_NULL;
  if (PMPI_Group_from_session_pset(pnmpi_session, pset, &group) != MPI_SUCCESS)
    {
      fprintf(stderr, "%s:%d: PMPI_Group_from_session_pset failed\n",
              __FUNCTION__, __LINE__);
      exit(EXIT_FAILURE);
    }

  MPI_Comm comm = MPI_COMM_NULL;
  if (PMPI_Comm_create_from_group(group, "pnmpi", MPI_INFO_NULL,
                                  MPI_ERRORS_ARE_FATAL, &comm) != MPI_SUCCESS)
    {
      fprintf(stderr, "%s:%d: PMPI_Comm_create_from_group failed\n",
              __FUNCTION__, __LINE__);
      exit(EXIT_FAILURE);
    }

  if (PMPI_Comm_free(&comm) != MPI_SUCCESS)
    {
      fprintf(stderr, "%s:%d: PMPI_Comm_free failed\n", __FUNCTION__, __LINE__);
      exit(EXIT_FAILURE);
    }

  if (PMPI_Group_free(&group) != MPI_SUCCESS)
    {
      fprintf(stderr, "%s:%d: PMPI_Comm_free failed\n", __FUNCTION__, __LINE__);
      exit(EXIT_FAILURE);
    }
}
#endif
#endif

/** \brief Initialize PnMPI.
 *
 * \note This function must be called before using any of PnMPI's API functions.
 *
 *
 * \private
 */
PNMPI_INTERNAL
void pnmpi_initialize(void)
{
  /* If PnMPI is already initialize, do not initialize it a second time. A
   * counter will be increased to be save how often the initialization has been
   * called, to be used later in the finalization. */
  if (pnmpi_initialized++)
    return;

  /* Call the PnMPI initialization functions. These will load the PnMPI
   * configuration, parse it and load all the modules defined in the config. */
  pnmpi_PreInit();

#if defined(PNMPI_ENABLE_SESSIONS)
  MPI_Info info = MPI_INFO_NULL;
  PMPI_Info_create(&info);
  PMPI_Info_set(info, "thread_level", "MPI_THREAD_MULTIPLE");
  if (PMPI_Session_init(info, MPI_ERRORS_ARE_FATAL, &pnmpi_session) !=
      MPI_SUCCESS)
    {
      {
        fprintf(stderr, "%s:%d: PMPI_Session_init failed\n", __FUNCTION__,
                __LINE__);
        exit(EXIT_FAILURE);
      }
    }
  PMPI_Info_free(&info);

#if defined(PNMPI_QUIRK_MPICH_SESSION_INIT_ORDER)
  // We create and discard the comms here on purpose. Getting the rank from
  // world group is sufficient but mpich 4.0 has a bug where it leaves the
  // builtin comms uninitialized if MPI_Session_init is called before any call
  // to MPI_Init_thread. Calling PMPI_Comm_create_from_group with the world pset
  // "reminds" mpich of the missing initialization and results in a usable world
  // model.
  pnmpi_comm_create_discard(pnmpi_session, "mpi://WORLD");
  pnmpi_comm_create_discard(pnmpi_session, "mpi://SELF");
#endif

  pnmpi_print_banner();
  pnmpi_initialization_complete = 1;
#endif
}


#ifdef __GNUC__
/** \brief The PnMPI constructor.
 *
 * \details If the compiler supports constructors, initialize PnMPI this early,
 *  so PnMPI is ready to be used, when `main()` gets called.
 *
 *
 * \param argc Count of \p argv.
 * \param argv The argument vector of the executable.
 *
 *
 * \private
 */
PNMPI_INTERNAL
__attribute__((constructor)) void pnmpi_constructor(PNMPI_UNUSED int argc,
                                                    PNMPI_UNUSED char **argv)
{
  pnmpi_initialize();
}
#endif
