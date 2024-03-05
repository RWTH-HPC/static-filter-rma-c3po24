/* This file is part of P^nMPI.
 *
 * Copyright (c)
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2011-2016 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2013-2022, RWTH Aachen University, Federal Republic of Germany
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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>
#include <pnmpi/limit-threading.h>
#include <pnmpi/service.h>


int main(int argc, char **argv)
{
  int ret;

#ifdef THREADED
  int provided;
  MPI_Init_thread(&argc, &argv, MPI_THREAD_FUNNELED, &provided);
#endif /* THREADED */
#ifdef UNTHREADED
  MPI_Init(&argc, &argv);
#endif /* UNTHREADED */

  PNMPI_modHandle_t module;
  ret = PNMPI_Service_GetModuleByName(PNMPI_MODULE_LIMIT_THREADING, &module);
  if (ret != PNMPI_SUCCESS)
    {
      fprintf(stderr, "Error: Could not get the module handle.\n");
      assert(0);
    }
  PNMPI_Global_descriptor_t buffer;
  ret = PNMPI_Service_GetGlobalByName(
    module, PNMPI_MODULE_LIMIT_THREADING_GLOBAL_REQUIRED, 'i', &buffer);
  if (ret != PNMPI_SUCCESS)
    {
      fprintf(stderr, "Error: Could not get the global.\n");
      assert(0);
    }
  PNMPI_threading_level_t actual_level = (int)*buffer.addr.i;
  printf("Required: %d\n", actual_level);

  MPI_Finalize();
  return EXIT_SUCCESS;
}

/* CONFIGS: unthreaded threaded
 *
 * DEPENDS: testbin-mpi-wrapper
 * COMPILE_INCLUDES: @CMAKE_CURRENT_BINARY_DIR@/../../src @MPI_C_INCLUDE_PATH@
 * LINK: pnmpi @MPI_C_LIBRARIES@
 * LINK_FLAGS: @MPI_C_LINK_FLAGS@
 *
 * RUN: @MPIEXEC@ @MPIEXEC_NUMPROC_FLAG@ 1
 * RUN:   @MPIEXEC_PREFLAGS@ @BINARY@ @MPIEXEC_POSTFLAGS@
 *
 * COMPILE_FLAGS: @MPI_C_COMPILE_FLAGS@
 * PNMPICONF: module limit-threading
 *
 * ENVIRONMENT-unthreaded: PNMPI_CONF=@PNMPICONF@
 * ENVIRONMENT-unthreaded: PNMPI_FORCE_THREADING_LEVEL=multiple
 * COMPILE_FLAGS-unthreaded: @MPI_C_COMPILE_FLAGS@ -DUNTHREADED
 * PASS-unthreaded: Required: 4
 *
 * ENVIRONMENT-threaded: PNMPI_CONF=@PNMPICONF@
 * ENVIRONMENT-threaded: PNMPI_FORCE_THREADING_LEVEL=multiple
 * COMPILE_FLAGS-threaded: @MPI_C_COMPILE_FLAGS@ -DTHREADED
 * PASS-threaded: Required: 1
 */
