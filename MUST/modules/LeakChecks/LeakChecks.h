/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file LeakChecks.h
 *       @see MUST::LeakChecks.
 *
 *  @date 17.05.2011
 *  @author Mathias Korepkat, Tobias Hilbrich
 */

#include "ModuleBase.h"
#include "CompletionTree.h"
#include "I_ParallelIdAnalysis.h"
#include "I_CreateMessage.h"
#include "I_CommTrack.h"
#include "I_DatatypeTrack.h"
#include "I_ErrTrack.h"
#include "I_GroupTrack.h"
#include "I_KeyvalTrack.h"
#include "I_OpTrack.h"
#include "I_RequestTrack.h"

#include "I_LeakChecks.h"

#include <string>

#ifndef LEAKCHECKS_H
#define LEAKCHECKS_H

using namespace gti;

namespace must
{
/**
 * Implementation of I_LeakChecks.
 */
class LeakChecks : public gti::ModuleBase<LeakChecks, I_LeakChecks>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    LeakChecks(const char* instanceName);

    /**
     * Destructor.
     */
    virtual ~LeakChecks(void);

    /**
     * @see I_LeakChecks::finalizeNotify.
     */
    GTI_ANALYSIS_RETURN finalizeNotify(I_ChannelId* thisChannel);

  protected:
    I_ParallelIdAnalysis* myPIdMod;
    I_CreateMessage* myLogger;
    I_CommTrack* myCTrack;
    I_DatatypeTrack* myDTrack;
    I_ErrTrack* myETrack;
    I_GroupTrack* myGTrack;
    I_KeyvalTrack* myKTrack;
    I_OpTrack* myOTrack;
    I_RequestTrack* myRTrack;

    CompletionTree*
        myFinCompletion; /**< Used to determine when the last finalize call arrives at this place.*/

    /**
     * Reports leaked communicators.
     */
    void reportComms(void);

    /**
     * Reports leaked datatypes.
     */
    void reportDatatypes(void);

    /**
     * Reports leaked errorhandlers.
     */
    void reportErrs(void);

    /**
     * Reports leaked groups.
     */
    void reportGroup(void);

    /**
     * Reports leaked keyvalue keys.
     */
    void reportKeys(void);

    /**
     * Reports leaked operations.
     */
    void reportOps(void);

    /**
     * Reports leaked requests.
     */
    void reportRequests(void);
};
} // namespace must

#endif /*LEAKCHECKS_H*/
