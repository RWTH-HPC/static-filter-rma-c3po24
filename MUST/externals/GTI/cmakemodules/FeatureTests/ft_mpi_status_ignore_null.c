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
 * Check whether MPI_STATUS_IGNORE is NULL
 *
 * - OpenMPI: true
 * - IntelMPI: false
 */

_Static_assert(MPI_STATUS_IGNORE == NULL, "MPI_STATUS_IGNORE is not NULL");

int main(int argc, char** argv) { return 0; }
