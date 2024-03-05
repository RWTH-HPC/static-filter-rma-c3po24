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

/* \brief Count MPI calls.
 *
 * \details This module counts how often each MPI call was invoked. Before MPI
 *  finalizes, statistics will be printed for each rank and a sum of the made
 *  calls by all ranks.
 */


#include <stdio.h>

#include <mpi.h>
#include <pnmpi/debug_io.h>
#include <pnmpi/hooks.h>
#include <pnmpi/private/pmpi_assert.h>
#include <pnmpi/xmpi.h>

#include "pnmpi-atomic.h"


/* If there is no atomic support, we'll limit the threading level of this module
 * to MPI_THREAD_SERIALIZED, so it is safe to use with threaded applications
 * (but they may become slower!). */
#if defined(METRIC_NO_ATOMIC)
const int PNMPI_SupportedThreadingLevel = MPI_THREAD_SERIALIZED;
#endif


/** \brief Struct of counters.
 *
 * \details This struct stores the counters for all MPI calls.
 */
enum counter_functions
{
  counter_INVALID=-1,
  {{forallfn fn_name MPI_Finalize}}
  counter_{{fn_name}},
  {{endforallfn}}
  counter_LAST
};

metric_atomic_keyword size_t counters[counter_LAST];


/** \brief Helper function to print counter struct.
 *
 * \details This function will print all functions called at least once with
 *  their counter.
 *
 *
 * \param c \ref counter struct to be printed.
 */
static void print_counters(size_t *c)
{
  {{forallfn fn_name MPI_Finalize}}
  if (c[counter_{{fn_name}}] > 0)
    printf("  %8zu %s\n", c[counter_{{fn_name}}], "{{fn_name}}");
  {{endforallfn}}
}


/* Generate wrapper functions for all MPI calls to increment the counter on
 * every call. MPI_Finalize will be ignored, because it will be used to print
 * the statistics below. */

{{fnall fn_name MPI_Finalize MPI_Pcontrol MPI_Init MPI_Init_thread}}
  metric_atomic_inc(counters[counter_{{fn_name}}]);

  WRAP_MPI_CALL_PREFIX
  X{{fn_name}}({{args}});
  WRAP_MPI_CALL_POSTFIX
{{endfnall}}

{{fn fn_name MPI_Init MPI_Init_thread}}
  for (int n = 0; n < counter_LAST; n++)
    counters[n] = 0;
  
  metric_atomic_inc(counters[counter_{{fn_name}}]);

  WRAP_MPI_CALL_PREFIX
  X{{fn_name}}({{args}});
  WRAP_MPI_CALL_POSTFIX
{{endfn}}



/* MPI_Pcontrol needs special handling, as it doesn't call a PMPI function and
 * PnMPI does not implement the required XMPI call. Instead PnMPI will act as a
 * multiplexer for MPI_Pcontrol, so all we have to do is increment the counter
 * and return. */

int MPI_Pcontrol(const int level, ...)
{
  metric_atomic_inc(counters[counter_MPI_Pcontrol]);

  return MPI_SUCCESS;
}


/** \brief Print the statistics.
 *
 * \details This function will print the statistics to stdout for each rank and
 *  a sum of all ranks.
 *
 * \todo In feature releases this should be done by the PNMPI_AppShutdown hook
 *  to track duplicated MPI_Finalize calls.
 *
 *
 * \return The return value of PMPI_Finalize will be pass through.
 */
int MPI_Finalize()
{
  /* Flush the buffers to avoid fragments in the output.
   *
   * NOTE: This can't fully prevent buffered output being displayed between
   *       fresh one, as there's no guaranty the MPI sends the buffers back
   *       to the host immediately. */
  fflush(stdout);
  fflush(stderr);


  int rank, size;

  PMPI_Comm_rank_assert(MPI_COMM_WORLD, &rank);
  PMPI_Comm_size_assert(MPI_COMM_WORLD, &size);

  size_t tmp_counters[counter_LAST];
  size_t total_counters[counter_LAST];
  int n;
  for (n = 0; n < counter_LAST; n++)
    total_counters[n] = tmp_counters[n] = counters[n];

  /* The entire output will be handled by rank 0 to avoid output collisions, as
   * the individual ranks can't sync their output being displayed at the
   * frontend. Therefore, rank 0 prints a header and its counters first. */
  if (rank == 0)
    {
      printf("\n\n################################\n\n"
             "MPI call stats:\n\n"
             " Rank 0:\n");
      print_counters(total_counters);
    }

  /* Rank 0 should receive the counters from other ranks now, to display those.
   * These will be summed up for the total counters printed below, to cache the
   * counters instead of using a reduction for receiving the counters of all
   * ranks a second time. */
  if (rank == 0)
    {
      for (n = 1; n < size; n++)
        {
          PMPI_Recv(tmp_counters, counter_LAST, MPI_UNSIGNED_LONG, n, 0,
                    MPI_COMM_WORLD, MPI_STATUS_IGNORE);
          for (int i = 0; i < counter_LAST; i++)
            total_counters[i] += tmp_counters[i];
          printf("\n Rank %d:\n", n);
          print_counters(tmp_counters);
        }
    }
  else
    {
      PMPI_Send(tmp_counters, counter_LAST, MPI_UNSIGNED_LONG, 0, 0,
                MPI_COMM_WORLD);
    }

  /* Print the total counters. These have been summed up in the counter struct
   * of rank 0, so these have not to be received a second time. */
  if (rank == 0)
    {
      printf("\n Total:\n");
      print_counters(total_counters);
      fflush(stdout);
    }


  /* Call original MPI_Finalize. */
  return XMPI_Finalize();
}
