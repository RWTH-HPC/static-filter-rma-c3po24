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

#define _GNU_SOURCE 1
#include <pnmpi/private/attributes.h>
#include <pnmpi/private/self_base_address.h>


/** \brief The base address of the PnMPI shared object file.
 *
 * \details This variable stores the base address at which the PnMPI shared object
 *  located. This is helpful when transferring function addresses between ranks,
 *  because ASLR might require to translate function addresses with the help of the
 *  base address of the PnMPI shared object.
 *
 * \note This value should not be changed directly by any function, but by using
 *  the \ref pnmpi_base_address_set function.
 *
 *
 * \private
 */
PNMPI_INTERNAL
void *pnmpi_self_base_address = NULL;

PNMPI_INTERNAL
void pnmpi_set_self_base_address(void *addr)
{
    Dl_info info;
    if (pnmpi_self_base_address == NULL && dladdr((void*)addr, &info) != 0) {
      pnmpi_self_base_address = info.dli_fbase;
    }
    
}