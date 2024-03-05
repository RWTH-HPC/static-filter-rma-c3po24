/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file I_Operation.h
 *       @see MUST::I_Operation.
 *
 *  @date 26.07.2011
 *  @author Tobias Hilbrich, Joachim Protze, Mathias Korepkat
 */

#include "GtiEnums.h"
#include "MustEnums.h"
#include <iostream>

#ifndef I_OPERATION_H
#define I_OPERATION_H

using namespace gti;

namespace must
{
/**
 * Interface for all operations that can be reordered.
 */
class I_Operation
{
  public:
    /**
     * Virtual destructor needed.
     */
    virtual ~I_Operation(){};

    /**
     * Called when a operation should be processed.
     * @param rank to which the operation belongs.
     * @return GTI_SUCCESS if successful.
     */
    virtual PROCESSING_RETURN process(int rank) = 0;

    /**
     * Called when human readable information on
     * an operation shall be printed. Primary use case
     * is for debugging.
     * @param out stream to print into.
     * @return GTI_SUCCESS if successful.
     */
    virtual GTI_RETURN print(std::ostream& out) = 0;

    /**
     * Copies an op that is within the I_OperationReordering queues.
     * If multiple ops are in the queues, the reordering will call this
     * function in the queueing order. This is helpful if the copy triggers
     * registration/deregistration of the operation in other order dependent
     * structures.
     * @return pointer to the copy.
     */
    virtual I_Operation* copyQueuedOp(void) = 0;
};
} /*namespace must*/

#endif /*I_OPERATION_H*/
