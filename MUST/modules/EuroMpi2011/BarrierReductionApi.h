/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file BarrierReductionApi.h
 * 		P call definition for barrier reduction EuroMPI2011 test case.
 *
 * @author Tobias Hilbrich
 * @date 06.05.2011
 */

#ifndef BARRIER_REDUCTION_API_H
#define BARRIER_REDUCTION_API_H

inline int PreducedBarrier(unsigned long long tMin, unsigned long long tMax) { return 0; }

typedef int (*reducedBarrierP)(unsigned long long tMin, unsigned long long tMax);

#endif /*BARRIER_REDUCTION_API_H*/
