/* This file is part of GTI (Generic Tool Infrastructure)
 *
 * Copyright (C)
 *  2008-2019 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2013-2019 RWTH Aachen University, Federal Republic of Germany
 *
 * See the LICENSE file in the package base directory for details
 */

/**
 * @file CStratCrashHandling.w
 * This is the source for the generated part included by CStratCrashHandling.cpp. 
 */

extern "C" {
{{fnalltype fn_name MPI_Comm* MPI_Comm_free MPI_Comm_disconnect MPI_Comm_idup}} {
 WRAP_MPI_CALL_PREFIX
   {{ret_val}} = X{{fn_name}}({{args}});
 WRAP_MPI_CALL_POSTFIX
   {{apply_to_type MPI_Comm* GTI_SET_ERR_HANDLER}}
}{{endfnalltype}}
}
