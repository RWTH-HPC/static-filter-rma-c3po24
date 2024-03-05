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

#ifndef _PNMPI_MOD_LIMIT_THREADING
#define _PNMPI_MOD_LIMIT_THREADING


#include <mpi.h>

/**
 * The registered name of the module..
 */
#define PNMPI_MODULE_LIMIT_THREADING "limit-threading"

/**
 * The name of the service's global variable that provides the thread level
 * which was actually requested by the application.
 *
 * The global variable has the type \ref PNMPI_threading_level_t.
 */
#define PNMPI_MODULE_LIMIT_THREADING_GLOBAL_REQUIRED "required-thread-level"

/** \brief Enum to define the MPI threading level defined by the user.
 *
 * \details This enum will be used as wrapper around MPI's threading level to
 *  define an extra value, if the user didn't define a threading level, that is
 *  not conflicting with the values of the MPI header.
 */
typedef enum threading_level
{
  /* NOTE: Instead of the following items of this enum, the original ones
   *       defined in the MPI header will be used in the code below. However,
   *       these are required to get a unique value for THREAD_LEVEL_NONE, that
   *       isn't in conflict with the regular MPI threading levels. */
  THREAD_LEVEL_SINGLE = MPI_THREAD_SINGLE,
  THREAD_LEVEL_FUNNELED = MPI_THREAD_FUNNELED,
  THREAD_LEVEL_SERIALIZED = MPI_THREAD_SERIALIZED,
  THREAD_LEVEL_MULTIPLE = MPI_THREAD_MULTIPLE,

  THREAD_LEVEL_NONE
  /**< No thread level has been defined in the environment or via an argument in
   *   PnMPI's configuration. */
} PNMPI_threading_level_t;

#endif // _PNMPI_MOD_LIMIT_THREADING
