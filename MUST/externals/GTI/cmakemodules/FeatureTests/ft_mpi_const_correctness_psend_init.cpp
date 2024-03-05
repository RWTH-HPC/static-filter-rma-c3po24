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
 * This test evaluates whether we require const correctness that was introduced with MPI 3.
 * The basic const correctness test is "ft_mpi_const_correctness.c".
 *
 * This test looks at an unclear case that differs depending on MPI,
 * which is MPI_Psend_init:
 * For MPICH 4.0.0 buf is *not* const
 * For MPICH 4.0.2 buf is const
 */

int main(int argc, char** argv)
{
    const int buf[3] = {1, 1, 1};
    MPI_Request req;
    MPI_Psend_init(buf, 3, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, MPI_INFO_NULL, &req);
    return 0;
}
