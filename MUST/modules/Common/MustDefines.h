/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file MustDefines.h
 *       @see MustDefines.
 *
 *  @date 17.08.2011
 *  @author Mathias Korepkat
 */

#include "mpi.h"
#include <stdlib.h>
#include <stdint.h>

#ifndef MUSTDEFINES_H
#define MUSTDEFINES_H

#define MUST_BOTTOM ((int64_t)-1L)
#define MUST_IN_PLACE ((int64_t)-2L)

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

/**
 * generate fortran bindings
 */
#define GENERATE_F77_BINDINGS(lower_case, upper_case, wrapper_function, signature, params)         \
    EXTERN void lower_case signature;                                                              \
    EXTERN void lower_case signature { wrapper_function params; }                                  \
    EXTERN void lower_case##_ signature;                                                           \
    EXTERN void lower_case##_ signature { wrapper_function params; }                               \
    EXTERN void lower_case##__ signature;                                                          \
    EXTERN void lower_case##__ signature { wrapper_function params; }                              \
    EXTERN void upper_case signature;                                                              \
    EXTERN void upper_case signature { wrapper_function params; }

#ifndef MUST_MAX_NUM_STACKLEVELS
#define MUST_MAX_NUM_STACKLEVELS 10
#endif

#ifndef MUST_MAX_TOTAL_INFO_SIZE
#define MUST_MAX_TOTAL_INFO_SIZE 4096
#endif

#endif /*MUSTDEFINES_H*/
