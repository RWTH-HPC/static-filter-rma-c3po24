/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file TSanInterceptorApi.h
 *      P call definition for MUST propagating TSan intrumented functions.
 *
 * @author Felix Tomski
 * @date 26.07.2023
 */

#include "MustEnums.h"
#include "MustTypes.h"
#include "BaseIds.h"

#ifndef TSANINTERCEPTORAPI_H
#define TSANINTERCEPTORAPI_H

/* Propagate ordinary read and writes of different sizes */
inline int
PpropagateTSanAccess(MustParallelId pId, void* pc, int8_t isRead, void* addr, int64_t count)
{
    return 0;
}

typedef int (
    *propagateTSanAccessP)(MustParallelId pId, void* pc, int8_t isRead, void* addr, int64_t count);

/* Propagate ordinary read and writes asl bulk in one array */
inline int PpropagateTSanAccessBulk(
    MustParallelId pId,
    void** readPc,
    size_t* readPcNum,
    void** readStartAddr,
    void** readEndAddr,
    size_t readLen,
    size_t readPcLen,
    void** writePc,
    size_t* writePcNum,
    void** writeStartAddr,
    void** writeEndAddr,
    size_t writeLen,
    size_t writePcLen)
{
    return 0;
}

typedef int (*propagateTSanAccessBulkP)(
    MustParallelId pId,
    void** readPc,
    size_t* readPcNum,
    void** readStartAddr,
    void** readEndAddr,
    size_t readLen,
    size_t readPcLen,
    void** writePc,
    size_t* writePcNum,
    void** writeStartAddr,
    void** writeEndAddr,
    size_t writeLen,
    size_t writePcLen);

/* Propagate atomic accesses of different sizes */
inline int PpropagateTSanAtomicAccess(
    MustParallelId pId,
    void* pc,
    int accessType,
    int morder,
    void* addr,
    int64_t count)
{
    return 0;
}

typedef int (*propagateTSanAtomicAccessP)(
    MustParallelId pId,
    void* pc,
    int accessType,
    int morder,
    void* addr,
    int64_t count);

#endif /* TSANINTERCEPTORAPI_H */