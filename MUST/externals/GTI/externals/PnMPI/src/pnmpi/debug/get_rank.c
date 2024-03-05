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

#include <stdio.h>
#include <stdlib.h>

#include "core.h"
#include <mpi.h>
#include <pnmpi/debug_io.h>
#include <pnmpi/private/attributes.h>
#include <pnmpi/private/config.h>
#include <pnmpi/private/print.h>

#if defined(PNMPI_ENABLE_SESSIONS)
/** Get the rank of this process.
 *
 * Requires that PnMPI's internal session is already initialized.
 *
 * @return The rank as in world comm or -1 on error.
 */
static int get_rank_from_session()
{
  if (pnmpi_session == MPI_SESSION_NULL)
    {
      return -1;
    }

  MPI_Group group_world = MPI_GROUP_NULL;
  if (PMPI_Group_from_session_pset(pnmpi_session, "mpi://WORLD",
                                   &group_world) != MPI_SUCCESS)
    {
      fprintf(stderr, "%s:%d: PMPI_Group_from_session_pset failed\n",
              __FUNCTION__, __LINE__);
      exit(EXIT_FAILURE);
    }
  int rank = -1;
  if (PMPI_Group_rank(group_world, &rank) != MPI_SUCCESS)
    {
      fprintf(stderr, "%s:%d: PMPI_Group_rank failed\n", __FUNCTION__,
              __LINE__);
      exit(EXIT_FAILURE);
    }
  if (PMPI_Group_free(&group_world) != MPI_SUCCESS)
    {
      fprintf(stderr, "%s:%d: PMPI_Group_free failed\n", __FUNCTION__,
              __LINE__);
      exit(EXIT_FAILURE);
    }
  return rank;
}
#else

/** Get the rank of this process.
 *
 * Requires MPI to be initialized with \ref MPI_Init or \ref MPI_Init_thread.
 *
 * @return The rank as in world comm or -1 on error.
 */
static int get_rank_from_comm_world()
{
  int rank = -1;
  /* If MPI was not initialized until now, we can't lookup the rank. */
  int initialized;
  if (PMPI_Initialized(&initialized) != MPI_SUCCESS)
    {
      /* pnmpi_error can't be used here, because this would result in an endless
       * loop: pnmpi_error is a wrapper of pnmpi_warning which uses this
       * function, so the PMPI_Initialized error would occur again and again
       * until the stack is full. */
      fprintf(stderr, "%s:%d: PMPI_Initialized failed\n", __FUNCTION__,
              __LINE__);
      exit(EXIT_FAILURE);
    }
  if (!initialized)
    return -1;

  /* If PnMPI is initialized get the rank of this process. If this fails,
   * print an error message and exit the application. */
  if (PMPI_Comm_rank(MPI_COMM_WORLD, &rank) != MPI_SUCCESS)
    {
      /* pnmpi_error can't be used here, because this would result in an
       * endless loop: pnmpi_error is a wrapper of pnmpi_warning which uses
       * this function, so the PMPI_Comm_rank error would occur again and
       * again until the stack is full. */
      fprintf(stderr, "%s:%d: PMPI_Comm_rank failed\n", __FUNCTION__, __LINE__);
      exit(EXIT_FAILURE);
    }

  return rank;
}
#endif

PNMPI_INTERNAL
int pnmpi_get_rank(void)
{
  /* Use the following variable as cache for the rank of this process. If the
   * rank was cached by a call before, return the cached result instead of
   * making a new lookup. */
  static int rank = -1;
  if (rank != -1)
    return rank;

#if defined(PNMPI_ENABLE_SESSIONS)
  rank = get_rank_from_session();
#else
  rank = get_rank_from_comm_world();
#endif
  return rank;
}

#if defined(PNMPI_ENABLE_SESSIONS)
/** \brief Get the size of the world communicator.
 *
 * Requires that PnMPI's internal session is already initialized.
 *
 * @return The size of the world communincator or 0 on error.
 */
static int pnmpi_get_world_size_from_session()
{
  if (pnmpi_session == MPI_SESSION_NULL)
    {
      return 0;
    }

  MPI_Group group = MPI_GROUP_NULL;
  if (PMPI_Group_from_session_pset(pnmpi_session, "mpi://WORLD", &group) !=
      MPI_SUCCESS)
    {
      /* pnmpi_error can't be used here, because this would result in an
       * endless loop: pnmpi_error is a wrapper of pnmpi_warning which
       * uses this function, so the PMPI_Comm_size error would occur again
       * and again until the stack is full. */
      fprintf(stderr, "%s:%d: PMPI_Group_from_session_pset failed\n",
              __FUNCTION__, __LINE__);
      exit(EXIT_FAILURE);
    }
  int size = 0;
  if (PMPI_Group_size(group, &size) != MPI_SUCCESS)
    {
      /* pnmpi_error can't be used here, because this would result in an
       * endless loop: pnmpi_error is a wrapper of pnmpi_warning which
       * uses this function, so the PMPI_Comm_size error would occur again
       * and again until the stack is full. */
      fprintf(stderr, "%s:%d: PMPI_Group_size failed\n", __FUNCTION__,
              __LINE__);
      exit(EXIT_FAILURE);
    }
  if (PMPI_Group_free(&group) != MPI_SUCCESS)
    {
      fprintf(stderr, "%s:%d: PMPI_Group_free failed\n", __FUNCTION__,
              __LINE__);
      exit(EXIT_FAILURE);
    }
  return size;
}
#else
/** \brief Get the size of the world communicator.
 *
 * Requires MPI to be initialized with \ref MPI_Init or \ref MPI_Init_thread.
 *
 * @return The size of the world communincator or 0 on error.
 */
static int pnmpi_get_world_size_from_comm_world()
{
  /* Check if MPI is initialized yet. */
  int initialized;
  if (PMPI_Initialized(&initialized) != MPI_SUCCESS)
    {
      /* pnmpi_error can't be used here, because this would result in an endless
       * loop: pnmpi_error is a wrapper of pnmpi_warning which uses this
       * function, so the PMPI_Initialized error would occur again and again
       * until the stack is full. */
      fprintf(stderr, "%s:%d: PMPI_Initialized failed\n", __FUNCTION__,
              __LINE__);
      exit(EXIT_FAILURE);
    }
  if (!initialized)
    {
      return 0;
    }

  int finalized;
  if (PMPI_Finalized(&finalized) != MPI_SUCCESS)
    {
      /* pnmpi_error can't be used here, because this would result in an endless
       * loop: pnmpi_error is a wrapper of pnmpi_warning which uses this
       * function, so the PMPI_Finalized error would occur again and again
       * until the stack is full. */
      fprintf(stderr, "%s:%d: PMPI_Finalized failed\n", __FUNCTION__, __LINE__);
      exit(EXIT_FAILURE);
    }
  if (finalized)
    {
      return 0;
    }

  int size;
  if (PMPI_Comm_size(MPI_COMM_WORLD, &size) != MPI_SUCCESS)
    {
      /* pnmpi_error can't be used here, because this would result in an
       * endless loop: pnmpi_error is a wrapper of pnmpi_warning which uses
       * this function, so the PMPI_Comm_size error would occur again and
       * again until the stack is full. */
      fprintf(stderr, "%s:%d: PMPI_Comm_size failed\n", __FUNCTION__, __LINE__);
      exit(EXIT_FAILURE);
    }
  return size;
}
#endif

PNMPI_INTERNAL
int pnmpi_get_world_size()
{
#if defined(PNMPI_ENABLE_SESSIONS)
  return pnmpi_get_world_size_from_session();
#else
  return pnmpi_get_world_size_from_comm_world();
#endif
}
