/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_Request.h
 *       @see I_Request.
 *
 *  @date 23.06.2011
 *  @author Tobias Hilbrich, Joachim Protze, Mathias Korepkat
 */

#include "BaseIds.h"
#include "I_Comm.h"
#include "I_Datatype.h"
#include "I_Destructable.h"
#include "MustTypes.h"
#include <vector>

#ifndef I_REQUEST_H
#define I_REQUEST_H

namespace must
{
/**
 * Enumeration of the kinds of requests we handle.
 */
enum MUST_REQUEST_KIND {
    MUST_REQUEST_P2P =
        0,             /**< Request for non-blocking or persistent point-to-point communications.*/
    MUST_REQUEST_COLL, /**< Request for a non-blocking collective operation.*/
    MUST_REQUEST_IO,   /**< Request for an asynchronous file I/O operation.*/
    MUST_REQUEST_RMA,  /**< Request for an asynchronous remote memory access.*/
    MUST_REQUEST_GREQUEST, /**< A generalized request.*/
    MUST_REQUEST_UNKNOWN   /**< Undefined kind of request.*/
};

/**
 * Interface for storage and accessing Information
 * on a request as defined in MPI.
 */
class I_Request
{
  public:
    /*
     * Basic information
     */
    virtual bool
    isActive(void) = 0; /**< True if the request is currently in an active communication.*/
    virtual bool isPersistent(void) = 0; /**< True if the request is persistent.*/
    virtual bool
    isPartitioned(void) = 0; /**< True if request is associated with partitioned communication */
    virtual bool
    isPartitionActive(int i) = 0; /**< True if specified partition is already marked as active */
    virtual std::pair<MustParallelId, MustLocationId>
    getActivePartitionInfo(int i) = 0; /**< Returns the pId and lId where the specified
                                          partition was set as active */
    virtual bool isSend(void) = 0; /**< True if this request is associated with a send operation,
                                      false if associated with a receive operation. Only set if
                                      isActive or isPersistent is true.*/
    virtual bool isNull(void) = 0; /**< True if this is MPI_REQUEST_NULL.*/
    virtual bool isCanceled(void) = 0;           /**< Set to true if the request was canceled.*/
    virtual bool isProcNull(void) = 0;           /**< Set to true if this is a no-op request using
                                                    MPI_PROC_NULL, it will still be marked as active.*/
    virtual MUST_REQUEST_KIND getKind(void) = 0; /**< Returns the kind of the request.*/

    /*
     * Information for partitioned communication requests.
     */
    virtual int getNumPartitions(void) = 0;
    virtual void setPartitionActive(
        int partition,
        MustParallelId pId,
        MustLocationId lId) = 0; /**< Sets partition as active in either Bitmap
                                    or vector if verbose */
    virtual int getBitmapCount(void) = 0;
    virtual void clearBitmap(void) = 0;

    /** Prints information about partitions marked in MPI_Pready* which are already active.
     * Designed for printing in a style that suits the usage
     * of CreateMessage.
     * @param conflictPartitions to be written about.
     * @param pId parallel Id of the call site.
     * @param lId location Id of the call site.
     * @param out stream to use for output.
     * @param pReferences current references to which any additional references for the new
     * handle will be added.
     * @return true if successful.
     */
    virtual bool printConflictPartitions(
        const std::vector<int>& conflictPartitions,
        MustParallelId pId,
        MustLocationId lId,
        std::stringstream& out,
        std::list<std::pair<MustParallelId, MustLocationId>>* pReferences) = 0;

    /*
     * Information for persistent requests only.
     */
    virtual int getCount(void) = 0;
    virtual I_Datatype* getDatatype(void) = 0;
    virtual I_DatatypePersistent* getDatatypeCopy(void) = 0;
    virtual int getTag(void) = 0;
    virtual I_Comm* getComm(void) = 0;
    virtual I_CommPersistent* getCommCopy(void) = 0;

    /* Information for a request that is a persistent receive. */
    virtual int getSource(void) = 0;

    /* Information for a request that is a persistent send. */
    virtual int getDest(void) = 0;
    virtual must::MustSendMode getSendMode(void) = 0;

    /*
     * History information.
     */
    virtual MustParallelId
    getCreationPId(void) = 0; /**< For persistent requests information for call that created the
                                 request, otherwise not set.*/
    virtual MustLocationId
    getCreationLId(void) = 0; /**< For persistent requests information for call that created the
                                 request, otherwise not set.*/

    virtual MustParallelId
    getActivationPId(void) = 0; /**< For active requests information for call that activated the
                                   request, otherwise not set.*/
    virtual MustLocationId
    getActivationLId(void) = 0; /**< For active requests information for call that activated the
                                   request, otherwise not set.*/

    virtual MustParallelId
    getCancelPId(void) = 0; /**< For active requests that where marked for cancelation, information
                               for call that was used to cancel, otherwise not set.*/
    virtual MustLocationId
    getCancelLId(void) = 0; /**< For active requests that where marked for cancelation, information
                               for call that was used to cancel, otherwise not set.*/

    /**
     * Prints information for a specified request.
     * Designed for printing in a style that suits the usage
     * of CreateMessage.
     *
     * @param out stream to use for output.
     * @param pReferences current references to which any additional references for the new handle
     * will be added.
     * @return true if successful.
     */
    virtual bool printInfo(
        std::stringstream& out,
        std::list<std::pair<MustParallelId, MustLocationId>>* pReferences) = 0;

    /**
     * Virtual destructor as needed
     */
    virtual ~I_Request(void) {}
}; /*class I_Request*/

/**
 * Interface for storage and accessing Information
 * on a request as defined in MPI. This is the persistent
 * version of the interface. The user needs to call I_RequestPersistent::erase
 * when he is finished with it.
 */
class I_RequestPersistent : public I_Request, public virtual I_Destructable
{
}; /*class I_RequestPersistent*/

} /*namespace must*/

#endif /*I_REQUEST_H*/
