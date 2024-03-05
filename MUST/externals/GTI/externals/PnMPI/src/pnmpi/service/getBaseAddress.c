/* This file is part of P^nMPI.
 *
 * Copyright (c)
 *  2008-2017 Lawrence Livermore National Laboratories, United States of America
 *  2011-2017 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2013-2017 RWTH Aachen University, Federal Republic of Germany
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

#include <pnmpi/private/self_base_address.h>
#include <pnmpi/service.h>


/** \brief Get the base address of the PnMPI shared object file.
 *
 * \details Upon initialization, PnMPI will store the base address
 *  of the shared object. This might be useful to translate function
 *  addresses in case of ASLR when transferring function addresses
 *  between processes.
 *
 *
 * \param [out] ptr Pointer where to store the address.
 *
 * \return \ref PNMPI_SUCCESS The address has been stored into \p ptr.
 *
 *
 * \ingroup PNMPI_Service_GetReturnAddress
 */
PNMPI_status_t PNMPI_Service_GetSelfBaseAddress(void **ptr)
{
  assert(ptr);


  *ptr = pnmpi_get_self_base_address();
  return PNMPI_SUCCESS;
}
