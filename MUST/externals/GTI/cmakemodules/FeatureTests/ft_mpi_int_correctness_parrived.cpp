/* This file is part of GTI (Generic Tool Infrastructure)
 *
 * Copyright (C)
 *  2008-2019 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2013-2019 RWTH Aachen University, Federal Republic of Germany
 *
 * See the LICENSE file in the package base directory for details
 */

#include <mpi.h>
#include <stddef.h>

/**
 * Check the signature of MPI_Parrived
 *
 * OpenMPI 5.0.0rc7 uses "MPI_Count partition"
 * The standard specifies "int partition" for the C binding
 */

extern "C" int MPI_Parrived(MPI_Request request, int partition, int* flag)
{
    PMPI_Parrived(request, partition, flag);
    return 0;
}

int main(int argc, char** argv) { return 0; }
