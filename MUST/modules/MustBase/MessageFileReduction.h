/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file MessageFileReduction.h
 *       Implementation for I_MessageFileReduction.
 */

#ifndef MUST_MESSAGEFILEREDUCTION_H
#define MUST_MESSAGEFILEREDUCTION_H

#include "CompletionTree.h"
#include "I_CreateMessage.h"
#include "I_MessageFileReduction.h"

#include "ModuleBase.h"

namespace must
{
/**
 * This class implements a way to distribute the @ref changeMessageFile event.
 *
 * @see I_MessageFileReduction
 */
class MessageFileReduction : public gti::ModuleBase<MessageFileReduction, I_MessageFileReduction>
{
  public:
    /**
     * Constructor.
     *
     *
     * @param instanceName The name of this instance.
     */
    MessageFileReduction(const char* instanceName);

    /**
     * @see I_MessageFileReduction::changeMessageFile
     */
    gti::GTI_ANALYSIS_RETURN changeMessageFile(size_t fileId, const char* filename, size_t len);

    /**
     * @see I_MessageFileReduction::changeMessageFileWrapper
     */
    gti::GTI_ANALYSIS_RETURN changeMessageFileWrapper(const char* filename);

    /**
     * @see I_MessageFileReduction::reduce
     */
    gti::GTI_ANALYSIS_RETURN reduce(
        size_t fileId,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels);

    /**
     * @see I_MessageFileReduction::filter
     */
    gti::GTI_ANALYSIS_RETURN filter(
        size_t fileId,
        const char* filename,
        size_t len,
        gti::I_ChannelId* thisChannel,
        std::list<gti::I_ChannelId*>* outFinishedChannels);

    /**
     * @see I_Reduction::timeout
     */
    void timeout(void);

  protected:
    /**
     * Reference to the @ref I_CreateMessage module used by this instance.
     */
    I_CreateMessage* myCreateMessageModule;

    /**
     * Handle of the @ref openMessageFile function.
     *
     * This module uses the @ref openMessageFile function generated by the
     * weaver XML files for triggering the open message file event in the GTI
     * event tree. This pointer stores the pointer to this function.
     */
    void (*fpOpen)(size_t, const char*, size_t);

    /**
     * Handle of the @ref closeMessageFile function.
     *
     * This module uses the @ref closeMessageFile function generated by the
     * weaver XML files for triggering the close message file event in the GTI
     * event tree. This pointer stores the pointer to this function.
     */
    void (*fpClose)(size_t);

    /**
     * The completion tree for the reduction.
     *
     * The completion tree stores the IDs of processes, which have already
     * triggered the reduction and met the reduction conditions across the
     * entire reduction tree.
     */
    gti::CompletionTree* reduceCompletion;

    /**
     * The list reduction partners.
     *
     * This list stores the list of reduction partners until all processes of a
     * certain level have triggered the reduction and met the reduction
     * conditions.
     */
    std::list<gti::I_ChannelId*> reducePartners;

    /**
     * Weather the reduction timed out.
     */
    bool reduceTimedOut;

    /**
     * The current filterId used for the filter.
     *
     * The first process with a fileId higher than this id, will be forwarded in
     * the GTI process tree and update this id.
     */
    size_t filterId;
};

} // namespace must

#endif
