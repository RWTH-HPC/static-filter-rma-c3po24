/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TransferReductionApi.h
 * 		P call definition for transfer statistics intervals of the IPDPS2012 tool.
 *
 * @author Tobias Hilbrich, Joachim Protze, Mathias Korepkat
 * @date 16.09.2011
 */

#ifndef TRANSFER_REDUCTION_API_H
#define TRANSFER_REDUCTION_API_H

#include <stdint.h>

inline int PstatisticsInterval(
    uint64_t sentP2P,
    uint64_t numP2P,
    uint64_t sentColl,
    uint64_t numColl,
    uint64_t duration)
{
    return 0;
}

typedef int (*statisticsIntervalP)(
    uint64_t sentP2P,
    uint64_t numP2P,
    uint64_t sentColl,
    uint64_t numColl,
    uint64_t duration);

#endif /*TRANSFER_REDUCTION_API_H*/
