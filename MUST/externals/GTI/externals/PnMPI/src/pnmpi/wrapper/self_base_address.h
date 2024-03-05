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

#ifndef PNMPI_PRIVATE_SELF_BASE_ADDRESS_H
#define PNMPI_PRIVATE_SELF_BASE_ADDRESS_H

#include <assert.h>
#include <dlfcn.h>
#include <link.h>
#include <stddef.h>

#include <pnmpi/private/attributes.h>
#include <pnmpi/private/tls.h>


extern void *pnmpi_self_base_address;


/** \brief Store the base address of the PnMPI shared object.
 *
 *
 *
 * \private
 */
PNMPI_UNUSED
void pnmpi_set_self_base_address(void *addr);


/** \brief Get the base address of the PnMPI shared object.
 *
 *
 * \return The address of the base address of the PnMPI shared object.
 *
 *
 * \private
 */
PNMPI_UNUSED
static void *pnmpi_get_self_base_address(void)
{
  return pnmpi_self_base_address;
}

#endif
