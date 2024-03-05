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

/** \class module_limit_threading
 *
 * \brief Module to limit the threading level of MPI applications or force a
 *  specific one on-demand.
 *
 * \details This module limits the required threading level of the MPI
 *  application to a value dynamically set in the environment or PnMPI's
 *  configuration file. Simply set `PNMPI_THREADING_LEVEL` in the environment or
 *  the `thread-level` argument of this module in the configuration to one of
 *  `single`, `funneled`, `serialized` or `multiple` to set the upper limit of
 *  the MPI threading level to be uses and the application will not exceed it.
 *
 *  A second way to use this module is to set a specific MPI level to be
 *  enforced by setting `PNMPI_FORCE_THREADING_LEVEL` in the environment or
 *  `force-thread-level` as argument in the configuration. Then, MPI will be
 *  initialized with this specific MPI threading level, even if the application
 *  requested a lower level of parallelism or even didn't call
 *  \ref MPI_Init_thread but just \ref MPI_Init.
 *
 * \note To get the original threading level in most of the other modules which
 *  may analyze its value, consider to use this module as one of the last in the
 *  module stack.
 */

#include "limit-threading.h"

#include <assert.h>
#include <stdlib.h>

#include <mpi.h>
#include <pnmpi/debug_io.h>
#include <pnmpi/private/config.h>
#include <pnmpi/service.h>
#include <pnmpi/xmpi.h>

/**
 * Expression-macro for the min function.
 */
#define min(a, b) a < b ? a : b

/**
 * Storage for the service's global.
 */
static PNMPI_threading_level_t actually_required = THREAD_LEVEL_NONE;


/** \brief Get the MPI threading level from the environment or PnMPI config.
 *
 * \details This function will check the environment and PnMPI's configuration
 *  for the MPI threading level should be used as limit or to be enforced.
 *
 * \note If the MPI level is defined in both environment and PnMPI's
 *  configuration, the value set in the environment will be preferred.
 *
 *
 * \param env_name The environment variable to be checked.
 * \param arg_name The configuration argument to be checked.
 *
 * \return The MPI threading level or \ref THREAD_LEVEL_NONE, if no MPI
 *  threading level was set.
 */
static PNMPI_threading_level_t get_threading_level(const char *env_name,
                                                   const char *arg_name)
{
  assert(env_name);
  assert(arg_name);


  /* The user may set an MPI threading level either in the environment or
   * PnMPI's configuration file. The former will override the latter. */
  const char *env = getenv(env_name);
  if (env == NULL)
    env = PNMPI_Service_GetArgumentSelf(arg_name);

  /* If the user has defined a MPI threading level, convert it to an integer
   * (defined in the threading_level enum) and return it. */
  if (env != NULL)
    {
      if (strcmp("single", env) == 0)
        return MPI_THREAD_SINGLE;
      else if (strcmp("funneled", env) == 0)
        return MPI_THREAD_FUNNELED;
      else if (strcmp("serialized", env) == 0)
        return MPI_THREAD_SERIALIZED;
      else if (strcmp("multiple", env) == 0)
        return MPI_THREAD_MULTIPLE;

      /* If the MPI threading level is not a valid one of the MPI standard, an
       * error message will be printed and the application exits. */
      else
        PNMPI_Error("MPI threading level must be one of single, funneled, "
                    "serialized or multiple.\n");
    }

  return THREAD_LEVEL_NONE;
}


/** \brief Get the MPI threading level to which the process should be limited.
 *
 * \return The MPI threading level or \ref THREAD_LEVEL_NONE, if there is no
 *  maximum MPI threading level.
 */
static PNMPI_threading_level_t get_threading_level_limit()
{
  return get_threading_level("PNMPI_THREADING_LEVEL", "thread-level");
}


/** /brief Get the MPI threading level to be enforced.
 *
 * \return The MPI threading level to be enforced or \ref THREAD_LEVEL_NONE, if
 *  no MPI threading level was set.
 */
static PNMPI_threading_level_t get_threading_level_force()
{
  return get_threading_level("PNMPI_FORCE_THREADING_LEVEL",
                             "force-thread-level");
}


/** \brief Initialize MPI and check if \p required matches \p provided.
 *
 * \details This tiny wrapper initializes MPI with the \p required MPI threading
 *  level and prints a warning, if the MPI environment didn't provide this level
 *  of parallelism. This function should be used, if the \p required MPI
 *  threading level should be enforced.
 *
 *
 * \param argc Pointer to argc of `main`.
 * \param argv Pointer to argv of `main`.
 * \param required Required threading level.
 * \param [out] provided The provided threading level returned by the MPI
 *  library.
 *
 * \return The return value of \ref XMPI_Init_thread will be passed through.
 */
static int XMPI_Init_thread_check(int *argc, char ***argv, int required,
                                  int *provided)
{
  /* NOTE: As NULL may be a valid value for argc and argv for the MPI library,
   *       these will not be asserted. However, as the value of provided will be
   *       checked, it must not be NULL. */
  assert(provided);


  int ret = XMPI_Init_thread(argc, argv, required, provided);
  if (*provided < required)
    PNMPI_Error("MPI threading level %d should be enforced, but MPI "
                "provides only %d.\n",
                required, *provided);
  return ret;
}


/** \brief MPI_Init_thread wrapper for limiting the \p required threading level
 *   or to enforce a specific one.
 *
 * \details This function will limit the \p required MPI threading level or set
 *  it to a specific value depending on the environment and configuration.
 *
 *
 * \param argc Pointer to argc of `main`.
 * \param argv Pointer to argv of `main`.
 * \param required Required threading level.
 * \param [out] provided The provided threading level returned by the MPI
 *  library.
 *
 * \return Status of `MPI_Init_thread` returned by the MPI library.
 */
int MPI_Init_thread(int *argc, char ***argv, int required, int *provided)
{
  /* Save level to the service's global variable as it was actually required by
   * the application. */
  actually_required = required;

  /* First of all, check if a specific MPI threading level should be enforced.
   * If it doesn't match the required level, it will be altered to the forced
   * one. */
  int level = get_threading_level_force();
  if ((level != THREAD_LEVEL_NONE) && (required != level))
    {
      PNMPI_Debug(PNMPI_DEBUG_CALL,
                  "Application asked for MPI threading level "
                  "%d, but %d should be enforced.\n",
                  required, level);

      /* Instead of just updating the required MPI level, call this tiny little
       * wrapper to check the threading level provided by MPI_Init_thread. If it
       * doesn't match the level to be enforced, it'll print a warning to inform
       * the user. */
      return XMPI_Init_thread_check(argc, argv, level, provided);
    }

  /* If the MPI threading level should be limited to a specific level and the
   * required level exceeds this limit, update it before passing it to the MPI
   * library below. */
  level = get_threading_level_limit();
  if ((level != THREAD_LEVEL_NONE) && (level < required))
    {
      PNMPI_Debug(PNMPI_DEBUG_CALL, "Limiting the threading level to %d.\n",
                  level);
      required = level;
      int res = XMPI_Init_thread(argc, argv, required, provided);
      *provided = min(required, *provided);
      return res;
    }

  return XMPI_Init_thread(argc, argv, required, provided);
}


/** \brief MPI_Init wrapper to enforce the MPI threading level.
 *
 * \details If the application does MPI initialization without multithreading
 *  enabled, this wrapper ensures a specific MPI level will be enforced if
 *  required.
 *
 *
 * \param argc Pointer to argc of `main`.
 * \param argv Pointer to argv of `main`.
 *
 * \return Status of `MPI_Init` or `MPI_Init_thread` returned by the MPI
 *  library.
 */
int MPI_Init(int *argc, char ***argv)
{
  /* Check if a specific MPI threading level should be enforced and route the
   * MPI call to the multithreaded initialization if required. */
  int level = get_threading_level_force();
  if (level != THREAD_LEVEL_NONE)
    {
      PNMPI_Debug(PNMPI_DEBUG_CALL,
                  "Application didn't initialize MPI "
                  "multithreaded. MPI threading level %d "
                  "will be enforced.\n",
                  level);

      int provided;
      return XMPI_Init_thread_check(argc, argv, level, &provided);
    }

  /* If no MPI threading level should be enforced, the call will be passed
   * through the stack and a classical MPI_Init will be performed without any
   * multithreading enabled. */
  return XMPI_Init(argc, argv);
}

#ifdef PNMPI_ENABLE_SESSIONS
/**
 * \brief Convert the enum value of an MPI thread level to its string
 * representation.
 *
 * @param level the enum value of the thread level
 * @return The corresponding thread level string
 * \private
 */
static char const *threading_level_to_str(PNMPI_threading_level_t level)
{
  switch (level)
    {
    case MPI_THREAD_SINGLE: return "MPI_THREAD_SINGLE";
    case MPI_THREAD_FUNNELED: return "MPI_THREAD_FUNNELED";
    case MPI_THREAD_SERIALIZED: return "MPI_THREAD_SERIALIZED";
    case MPI_THREAD_MULTIPLE: return "MPI_THREAD_MULTIPLE";
    default: return "THREAD_LEVEL_NONE";
    }
}

/**
 * \brief Convert the string of an MPI thread level to the enum
 * representation.
 *
 * @param value zero-terminated string of the MPI thread level.
 * @return The corresponding thread level integer or `THREAD_LEVEL_NONE` if the
 * input string can not be recognized.
 * \private
 */
static PNMPI_threading_level_t str_to_threading_level(const char *value)
{
  if (strcmp("MPI_THREAD_SINGLE", value) == 0)
    {
      return THREAD_LEVEL_SINGLE;
    }
  if (strcmp("MPI_THREAD_FUNNELED", value) == 0)
    {
      return THREAD_LEVEL_FUNNELED;
    }
  if (strcmp("MPI_THREAD_SERIALIZED", value) == 0)
    {
      return THREAD_LEVEL_SERIALIZED;
    }
  if (strcmp("MPI_THREAD_MULTIPLE", value) == 0)
    {
      return THREAD_LEVEL_MULTIPLE;
    }
  return THREAD_LEVEL_NONE;
}

/** \brief Extract the threading level from an MPI_Info
 * object.
 *
 * If the key "thread_level" is not present, then PnMPI's THREAD_LEVEL_NONE is
 * returned.
 *
 * @param info_used the MPI_Info object handle
 * @return the threading level in in enum representation
 *
 * \private
 */
static PNMPI_threading_level_t level_from_info(MPI_Info info_used)
{
  // MPI_THREAD_SERIALIZED is the longest identifier string
  unsigned const THREAD_LEVEL_BUFFER_SIZE = sizeof("MPI_THREAD_SERIALIZED");

  int present = 0;
  char provided_str[THREAD_LEVEL_BUFFER_SIZE];
  int buflen = THREAD_LEVEL_BUFFER_SIZE;
  XMPI_Info_get_string(info_used, "thread_level", &buflen, provided_str,
                       &present);
  if (!present)
    {
      return THREAD_LEVEL_NONE;
    }
  return str_to_threading_level(provided_str);
}

/** \brief Copy an MPI_Info object and set the threading level of the copy to
 * the given value.
 *
 * Sets the key "thread_level" as used in the standard.
 *
 * @param info Handle of the Info object to copy
 * @param level The thread level to insert in to the copied Info object. Must
 * be a valid MPI thread level value.
 * @return Handle to the modified MPI_Info object copy.
 * \private
 */
static MPI_Info info_copy_with_thread_level(MPI_Info info,
                                            PNMPI_threading_level_t level)
{
  MPI_Info modified = MPI_INFO_NULL;
  XMPI_Info_dup(info, &modified);
  XMPI_Info_set(modified, "thread_level", threading_level_to_str(level));
  return modified;
}

/** \brief Initialize an MPI Session and check if required thread level matches
 * the provided one.
 *
 * \return The return value of \ref XMPI_Session_init will be passed through.
 * \private
 */
static int XMPI_Session_init_check(MPI_Info info, MPI_Errhandler errhandler,
                                   MPI_Session *session)
{
  assert(info != MPI_INFO_NULL);
  PNMPI_threading_level_t const required = level_from_info(info);

  int ret = XMPI_Session_init(info, errhandler, session);

  MPI_Info session_info = MPI_INFO_NULL;
  XMPI_Session_get_info(*session, &session_info);

  PNMPI_threading_level_t const provided = level_from_info(session_info);
  if (provided < required)
    {
      PNMPI_Error("MPI threading level %d should be enforced, but MPI provides "
                  "only %d.\n",
                  required, provided);
    }
  return ret;
}

/** \brief MPI_Session_init wrapper.
 */
int MPI_Session_init(MPI_Info info, MPI_Errhandler errhandler,
                     MPI_Session *session)
{
  PNMPI_threading_level_t required = THREAD_LEVEL_NONE;
  if (info != MPI_INFO_NULL)
    {
      required = level_from_info(info);
    }
  /* Save level to the service's global variable as it was actually required by
   * the application. */
  actually_required = required;

  /* First of all, check if a specific MPI threading level should be enforced.
   * If it doesn't match the required level, it will be altered to the forced
   * one. */
  PNMPI_threading_level_t level = get_threading_level_force();
  if ((level != THREAD_LEVEL_NONE) && (required != level))
    {
      PNMPI_Debug(PNMPI_DEBUG_CALL,
                  "Application asked for MPI threading level "
                  "%d, but %d should be enforced.\n",
                  required, level);

      MPI_Info modified = info_copy_with_thread_level(info, level);
      int ret = XMPI_Session_init_check(modified, errhandler, session);
      XMPI_Info_free(&modified);
      return ret;
    }

  /* If the MPI threading level should be limited to a specific level and the
   * required level exceeds this limit, update it before passing it to the MPI
   * library below. */
  level = get_threading_level_limit();
  if ((level != THREAD_LEVEL_NONE) && (level < required))
    {
      PNMPI_Debug(PNMPI_DEBUG_CALL, "Limiting the threading level to %d.\n",
                  level);

      MPI_Info modified = info_copy_with_thread_level(info, level);
      int ret = XMPI_Session_init(modified, errhandler, session);
      XMPI_Info_free(&modified);
      return ret;
    }

  return XMPI_Session_init(info, errhandler, session);
}

/** \brief MPI_Session_get_info wrapper
 */
int MPI_Session_get_info(MPI_Session session, MPI_Info *info_used)
{
  int ret = XMPI_Session_get_info(session, info_used);

  PNMPI_threading_level_t forced_level = get_threading_level_force();
  if (forced_level != THREAD_LEVEL_NONE)
    {
      // Enforcing the thread level has precedence over limiting. The provided
      // level in the info object should be already at least as high as
      // requested.
      return ret;
    }

  PNMPI_threading_level_t limited_level = get_threading_level_limit();
  if (limited_level != THREAD_LEVEL_NONE)
    {
      // We need to replace the MPI_Info object of the session in case the mpi
      // library gives us a higher level than requested. (This has been observed
      // with mpich and MPI Sessions.)
      PNMPI_threading_level_t const provided = level_from_info(*info_used);
      PNMPI_threading_level_t const target = min(provided, limited_level);
      MPI_Info const modified = info_copy_with_thread_level(*info_used, target);
      XMPI_Info_free(info_used);
      *info_used = modified;
      return ret;
    }

  return ret;
}
#endif

void PNMPI_RegistrationPoint()
{
  /* register this module and its services */

  PNMPI_status_t err =
    PNMPI_Service_RegisterModule(PNMPI_MODULE_LIMIT_THREADING);
  if (err != PNMPI_SUCCESS)
    {
      return;
    }

  /* Register the global int variable "required-thread-level". */
  PNMPI_Global_descriptor_t global;
  strcpy(global.name, PNMPI_MODULE_LIMIT_THREADING_GLOBAL_REQUIRED);
  global.addr.i = (int *)&actually_required;
  global.sig = 'i';
  err = PNMPI_Service_RegisterGlobal(&global);
  if (err != PNMPI_SUCCESS)
    {
      return;
    }
}
