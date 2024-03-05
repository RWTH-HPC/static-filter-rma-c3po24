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
#include "I_Request.h"
#include <atomic>
#include <cstdint>
#include <vector>

#ifndef REQUEST_H
#define REQUEST_H

namespace must
{
/**
 * Implementation of I_Comm (and I_CommPersistent).
 */
class Request : public I_RequestPersistent, public HandleInfoBase
{
  public:
    /**
     * Constructor.
     * Initializes as a MPI_REQUEST_NULL info.
     */
    Request();

    /**
     * Destructor.
     */
    ~Request();

    bool isActive(void);           /**< @see Request::isActive.*/
    bool isPersistent(void);       /**< @see Request::isPersistent.*/
    int getNumPartitions(void);    /**< @see Request::getNumPartitions.*/
    bool isPartitioned(void);      /**< @see Request::isPartitioned. */
    bool isPartitionActive(int i); /**< @see Request::isPartitionActive. */
    std::pair<MustParallelId, MustLocationId>
    getActivePartitionInfo(int i); /**< @see Request::getActivePartitionInfo. */
    void setPartitionActive(
        int partition,
        MustParallelId pId,
        MustLocationId lId); /**< @see Request::setPartitionActive. */
    bool printConflictPartitions(
        const std::vector<int>& conflictPartitions,
        MustParallelId pId,
        MustLocationId lId,
        std::stringstream& out,
        std::list<std::pair<MustParallelId, MustLocationId>>*
            pReferences);            /**< @see Request::printConflictPartitions. */
    int getBitmapCount(void);        /**< @see Request::getBitmapCount.*/
    void clearBitmap(void);          /**< @see Request::clearBitmap.*/
    bool isSend(void);               /**< @see Request::isSend.*/
    bool isNull(void);               /**< @see Request::isNull.*/
    bool isCanceled(void);           /**< @see Request::isCanceled.*/
    bool isProcNull(void);           /**< @see Request::isProcNull.*/
    MUST_REQUEST_KIND getKind(void); /**< @see Request::getKind.*/
    int getCount(void);              /**< @see Request::getCount.*/
    I_Datatype* getDatatype(void);   /**< @see Request::getDatatype.*/
    I_DatatypePersistent* getDatatypeCopy(void);
    int getTag(void);      /**< @see Request::getTag.*/
    I_Comm* getComm(void); /**< @see Request::getComm.*/
    I_CommPersistent* getCommCopy(void);
    int getSource(void);                   /**< @see Request::getSource.*/
    int getDest(void);                     /**< @see Request::getDest.*/
    must::MustSendMode getSendMode(void);  /**< @see Request::getSendMode.*/
    MustParallelId getCreationPId(void);   /**< @see Request::getCreationPId.*/
    MustLocationId getCreationLId(void);   /**< @see Request::getCreationLId.*/
    MustParallelId getActivationPId(void); /**< @see Request::getActivationPId.*/
    MustLocationId getActivationLId(void); /**< @see Request::getActivationLId.*/
    MustParallelId getCancelPId(void);     /**< @see Request::getCancelPId.*/
    MustLocationId getCancelLId(void);     /**< @see Request::getCancelLId.*/

    bool printInfo(
        std::stringstream& out,
        std::list<std::pair<MustParallelId, MustLocationId>>*
            pReferences); /**< @see Request::printInfo.*/

    std::string getResourceName(void); /**< @see HandleInfoBase::getResourceName.*/

  public:
    bool myIsActive;
    bool myIsPartitioned;
    bool myIsPersistent;
    bool myIsSend;
    bool myIsNull;
    bool myIsCanceled;
    bool myIsProcNull;
    MUST_REQUEST_KIND myKind;

    MustInfoType myInfo;

    // If isPersistent or Partitioned
    int myCount;
    I_DatatypePersistent* myDatatype;
    int myTag;
    I_CommPersistent* myComm;
    int myDestSource;
    must::MustSendMode mySendMode;

    // If is Persistent
    MustParallelId myCreationPId;
    MustLocationId myCreationLId;

    // If isActive
    MustParallelId myActivationPId;
    MustLocationId myActivationLId;

    // If isCanceled
    MustParallelId myCancelPId;
    MustLocationId myCancelLId;

}; /*class Request*/
} /*namespace must*/

#endif /*REQUEST_H*/
