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

/** \class module_wait_for_debugger
 *
 * \brief Module to help for using debuggers.
 *
 * \details This module provides some functionality to help users to debug their
 *  code. The PID and hostname of each rank will be printed, so the user may
 *  attach to the rank with a debugger. In addition the ranks may wait some time
 *  for the user.
 *
 * \note To get the data as early as possible, this module should be loaded
 *  first in the module stack.
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>

#include <mpi.h>
#include <pnmpi/debug_io.h>
#include <pnmpi/private/pmpi_assert.h>
#include <pnmpi/private/config.h>
#include <pnmpi/xmpi.h>


/* OSX does not support HOST_NAME_MAX in limits.h. We'll ensure that it'll be
 * defined with the code below. */
#ifndef HOST_NAME_MAX
#if defined(_POSIX_HOST_NAME_MAX)
#define HOST_NAME_MAX _POSIX_HOST_NAME_MAX
#elif defined(MAXHOSTNAMELEN)
#define HOST_NAME_MAX 255
#endif
#endif

/** \brief Print a message to stdout and wait for a specific time.
 *
 * \details This function will be called by the \ref MPI_Init and \ref
 *  MPI_Init_thread wrappers and prints the PID and (if it can be determined)
 *  the hostname of the rank.
 *
 * \param rank The rank of this process in the world communicator.
 *
 * \memberof module_wait_for_debugger
 * \private
 */
static void wait_for_debugger(int rank)
{
  static int waited = 0;
  if (waited)
    return;
  waited = 1;

  char hostname[HOST_NAME_MAX];
  if (gethostname(hostname, HOST_NAME_MAX) != 0)
    {
      PNMPI_Warning("Unable to get hostname.\n");
      hostname[0] = '\0';
    }

  printf("Rank %i%s%s has pid %i.\n", rank, (hostname[0] != '\0') ? " at " : "",
         (hostname[0] != '\0') ? hostname : "", getpid());
  fflush(stdout);


  /* If the WAIT_AT_STARTUP variable is defined, the number of seconds defined
   * will be waited, so one may attach with a debugger in this time. */
  if (getenv("WAIT_AT_STARTUP") != NULL)
    {
      int wait = atoi(getenv("WAIT_AT_STARTUP"));
      if (rank == 0)
        {
          printf("Waiting for %i seconds ...\n", wait);
          fflush(stdout);
        }

      sleep(wait);

      if (rank == 0)
        printf("Done waiting.\n");
    }
}

/** Determine the rank in MPI_COMM_WORLD using the MPI World Model.
 *
 * @return the rank in MPI_COMM_WORLD
 */
static int world_model_get_rank()
{
  int rank;
  PMPI_Comm_rank_assert(MPI_COMM_WORLD, &rank);
  return rank;
}

/** \brief MPI_Init wrapper.
 */
int MPI_Init(int *argc, char ***argv)
{
  int ret = XMPI_Init(argc, argv);
  wait_for_debugger(world_model_get_rank());
  return ret;
}


/** \brief MPI_Init_thread wrapper.
 */
int MPI_Init_thread(int *argc, char ***argv, int required, int *provided)
{
  int ret = XMPI_Init_thread(argc, argv, required, provided);
  wait_for_debugger(world_model_get_rank());
  return ret;
}

#ifdef PNMPI_ENABLE_SESSIONS
/** Determine the rank in the mpi://WORLD process set using the MPI Session
 * model.
 *
 * \param session handle of an initialized session
 * @return the rank in the mpi://WORLD pset
 */
static int session_model_get_rank(MPI_Session session)
{
  MPI_Group world_group = MPI_GROUP_NULL;
  XMPI_Group_from_session_pset(session, "mpi://WORLD", &world_group);
  int rank;
  XMPI_Group_rank(world_group, &rank);
  XMPI_Group_free(&world_group);
  return rank;
}

/** \brief MPI_Session_init wrapper.
 */
int MPI_Session_init(MPI_Info info, MPI_Errhandler errhandler,
                     MPI_Session *session)
{
  int ret = XMPI_Session_init(info, errhandler, session);
  wait_for_debugger(session_model_get_rank(*session));
  return ret;
}
#endif
