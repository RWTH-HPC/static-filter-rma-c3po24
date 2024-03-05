/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_Request.h
 *       @see I_Request.
 *
 *  @date 15.07.2011
 *  @author Tobias Hilbrich, Joachim Protze, Mathias Korepkat
 */

#include "Bitmap.h"
#include "HandleInfoBase.h"
#include "Request.h"
#include <atomic>
#include <cstdint>
#include <vector>

#ifndef PARTITIONEDREQUEST_H
#define PARTITIONEDREQUEST_H

namespace must
{
/**
 * Implementation of I_Comm (and I_CommPersistent).
 */
class PartitionedRequest : public Request
{
  public:
    /**
     * Constructor.
     * Initializes as a MPI_REQUEST_NULL info.
     */
    PartitionedRequest();

    /**
     * Destructor.
     */
    ~PartitionedRequest();

    bool isPartitionActive(int i); /**< @see Request::isPartitionActive. */
    std::pair<MustParallelId, MustLocationId>
    getActivePartitionInfo(int i); /**< @see Request::getActivePartitionInfo. */
    void setPartitionActive(
        int partition,
        MustParallelId pId,
        MustLocationId lId);    /**< @see Request::setPartitionActive. */
    int getNumPartitions(void); /**< @see Request::getNumPartitions.*/
    bool printConflictPartitions(
        const std::vector<int>& conflictPartitions,
        MustParallelId pId,
        MustLocationId lId,
        std::stringstream& out,
        std::list<std::pair<MustParallelId, MustLocationId>>*
            pReferences);     /**< @see Request::printConflictPartitions. */
    int getBitmapCount(void); /**< @see Request::getBitmapCount.*/
    void clearBitmap(void);   /**< @see Request::clearBitmap.*/
    bool isSend(void);        /**< @see Request::isSend.*/
    Bitmap& getBitmap(void);

    bool printInfo(
        std::stringstream& out,
        std::list<std::pair<MustParallelId, MustLocationId>>*
            pReferences); /**< @see Request::printInfo.*/

    std::string getResourceName(void); /**< @see HandleInfoBase::getResourceName.*/

  public:
    // If isPartitioned
    int myNumPartitions;
    // MustInfoType myInfo;
    Bitmap myMap;
    std::vector<std::pair<MustParallelId, MustLocationId>> myActivePartitions;

}; /*class Request*/
} /*namespace must*/

#endif /*REQUEST_H*/
