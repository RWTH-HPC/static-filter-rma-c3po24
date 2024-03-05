/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file OneSidedChecksApi.h
 * 		P call definition for MUST OneSidedChecks API calls.
 *
 * @author Simon Schwitanski
 * @date 13.06.2017
 */

#ifndef ONESIDEDCHECKS_API_H
#define ONESIDEDCHECKS_API_H

#include "BaseIds.h"

inline int PpassTargetRMAOpAcross(
    int origin,
    MustRMAId rmaId,
    MustParallelId pId,
    MustLocationId lId,
    bool isStore,
    bool isAtomic,
    bool isLocked,
    int target,
    int disp,
    int count,
    MustRemoteIdType originDatatype,
    MustRemoteIdType targetDatatype,
    MustRemoteIdType win,
    int epoch,
    unsigned long long clockValue,
    unsigned long long* vectorClock,
    size_t vectorClockSize)
{
    return 0;
}

typedef int (*passTargetRMAOpAcrossP)(
    int origin,
    MustRMAId rmaId,
    MustParallelId pId,
    MustLocationId lId,
    bool isStore,
    bool isAtomic,
    bool isLocked,
    int target,
    int disp,
    int count,
    MustRemoteIdType originDatatype,
    MustRemoteIdType targetDatatype,
    MustRemoteIdType win,
    int epoch,
    unsigned long long clockValue,
    unsigned long long* vectorClock,
    size_t vectorClockSize,
    int toPlaceId);

inline int PpassTargetCompletionAcross(
    MustParallelId pId,
    MustLocationId lId,
    int origin,
    int target,
    MustRemoteIdType win,
    int isLocalOnly,
    MustRMAId rmaId,
    int epoch)
{
    return 0;
}

typedef int (*passTargetCompletionAcrossP)(
    MustParallelId pId,
    MustLocationId lId,
    int origin,
    int target,
    MustRemoteIdType win,
    int epoch,
    int isLocalOnly,
    MustRMAId rmaId,
    int toPlaceId);

inline void PnotifySync(int remoteRank, int ownRank) {}

typedef void (*notifySyncP)(int remoteRank, int ownRank);

inline void PnotifyOriginOpStart(MustRMAId rmaId) {}

typedef void (*notifyOriginOpStartP)(MustRMAId rmaId);

inline void
PnotifyOriginOpComplete(MustParallelId pId, MustLocationId lId, MustRMAId* rmaId, int rmaIdLen)
{
}

typedef void (*notifyOriginOpCompleteP)(
    MustParallelId pId,
    MustLocationId lId,
    MustRMAId* rmaId,
    int rmaIdLen);

inline void PnotifyTargetOpStart(MustRMAId rmaId) {}

typedef void (*notifyTargetOpStartP)(MustRMAId rmaId);

inline void
PnotifyTargetOpComplete(MustParallelId pId, MustLocationId lId, MustRMAId* rmaId, int rmaIdLen)
{
}

typedef void (*notifyTargetOpCompleteP)(
    MustParallelId pId,
    MustLocationId lId,
    MustRMAId* rmaId,
    int rmaIdLen);

inline void PpropagateWinLock(
    MustParallelId pId,
    MustLocationId lId,
    int lock_type,
    int rank,
    MustWinType win,
    void* ann)
{
}

typedef void (*propagateWinLockP)(
    MustParallelId pId,
    MustLocationId lId,
    int lock_type,
    int rank,
    MustWinType win,
    void* ann);

inline void
PpropagateWinUnlock(MustParallelId pId, MustLocationId lId, int rank, MustWinType win, void* ann)
{
}

typedef void (*propagateWinUnlockP)(
    MustParallelId pId,
    MustLocationId lId,
    int rank,
    MustWinType win,
    void* ann);

inline void PpropagateSignal(void* ann) {}

typedef void (*propagateSignalP)(void* ann);

#endif
