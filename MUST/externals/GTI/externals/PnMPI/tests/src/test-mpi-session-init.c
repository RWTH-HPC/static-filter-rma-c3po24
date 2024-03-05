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

#include <mpi.h>
#include <stdlib.h> // EXIT_* macros
#include <string.h>

#include "mpi_errors.h"


static int str_to_mpi_thread_level(const char *value)
{
  if (strcmp("MPI_THREAD_SINGLE", value) == 0)
    {
      return MPI_THREAD_SINGLE;
    }
  if (strcmp("MPI_THREAD_FUNNELED", value) == 0)
    {
      return MPI_THREAD_FUNNELED;
    }
  if (strcmp("MPI_THREAD_SERIALIZED", value) == 0)
    {
      return MPI_THREAD_SERIALIZED;
    }
  if (strcmp("MPI_THREAD_MULTIPLE", value) == 0)
    {
      return MPI_THREAD_MULTIPLE;
    }
  return -1;
}


int main(int argc, char **argv)
{
  MPI_Info requested = MPI_INFO_NULL;
  MPI_Info_create(&requested);
  char const *const required = "MPI_THREAD_MULTIPLE";
  MPI_Info_set(requested, "thread_level", required);
  /* Initialize and finalize MPI. This should be enough to call all PnMPI setup
   * routines except the wrapper functions. */
  MPI_Session session;
  MPI_Session_init(requested, MPI_ERRORS_RETURN, &session);

  MPI_Info provided = MPI_INFO_NULL;
  MPI_Session_get_info(session, &provided);
  char buf[sizeof("MPI_THREAD_SERIALIZED")];
  int buf_len = sizeof(buf);
  int present = 0;
  MPI_Info_get_string(provided, "thread_level", &buf_len, buf, &present);
  printf("Required: %d Provided: %d\n", str_to_mpi_thread_level(required),
         str_to_mpi_thread_level(buf));

  MPI_Session_finalize(&session);

  /* In standard C the following return is not required, but in some situations
   * older versions of mpiexec report the job aborted, so the test case will
   * fail, even if it succeed. Returning EXIT_SUCCESS avoids this false error
   * message. */
  MPI_Info_free(&requested);
  return EXIT_SUCCESS;
}


/* Note: There is no special test for preloading PnMPI by environment variables,
 *       as different MPI implementations handle environment variables in
 *       different ways and the setting the variables for the whole test
 *       environment interferes with additional tools like AddressSanitizer, as
 *       it would be preloaded for mpiexec, too. However, preloading is
 *       indirectly tested by the PnMPIze tests.
 *
 *
 * CONFIGS: dynamic static
 *
 * DEPENDS: testbin-mpi-wrapper
 * COMPILE_INCLUDES: @CMAKE_CURRENT_BINARY_DIR@ @MPI_C_INCLUDE_PATH@
 * COMPILE_FLAGS: @MPI_C_COMPILE_FLAGS@
 * LINK_FLAGS: @MPI_C_LINK_FLAGS@
 *
 * RUN: @MPIEXEC@ @MPIEXEC_NUMPROC_FLAG@ 1
 * RUN:   @MPIEXEC_PREFLAGS@ @BINARY@ @MPIEXEC_POSTFLAGS@
 * PASS: No modules loaded.
 *
 *
 * LINK-dynamic: pnmpi @MPI_C_LIBRARIES@
 *
 * LINK-static: pnmpi_static @MPI_C_LIBRARIES@ dl m
 */
