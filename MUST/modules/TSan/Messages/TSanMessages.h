/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef TSANMESSAGES_H
#define TSANMESSAGES_H

/**
 * @file TSanMessages.h
 *       @see MUST::TSanMessages.
 *
 *  @date 23.11.2017
 *  @author Felix Dommes
 */

#include "BaseApi.h"
#include "I_CreateMessage.h"
#include "I_GenerateLocationId.h"
#include "I_ParallelIdAnalysis.h"
#include "I_TSanMessages.h"
#include "ModuleBase.h"

#include "tsan_report_desc.hpp"
namespace __tsan
{
class ReportDesc;
}

namespace must
{
/**
 * Adapter to pipe TSan messages into MUST-messages
 */
class TSanMessages : public gti::ModuleBase<TSanMessages, I_TSanMessages>
{
  public:
    /**
     * Constructor.
     * @param instanceName name of this module instance.
     */
    explicit TSanMessages(const char* instanceName);

    /**
     * Destructor.
     */
    ~TSanMessages() override;

    /**
     * Creates messages from TSan reports.
     *
     * @param report pointer to the Thread Sanitizer report
     * @return see gti::GTI_ANALYSIS_RETURN.
     */
    template <__tsan::TSanVersion T>
    gti::GTI_ANALYSIS_RETURN tsanReport(const __tsan::ReportDescT<T>* report);

    gti::GTI_ANALYSIS_RETURN fini() override;

  private:
    template <__tsan::TSanVersion T>
    std::string format(
        const __tsan::ReportDescT<T>* const report,
        std::list<std::pair<MustParallelId, MustLocationId>>& refList);
    I_CreateMessage* myLogger;
    I_GenerateLocationId* myGenLId;
    I_ParallelIdAnalysis* myPIdMod;
    handleNewLocationP myNewLocFunc;
};
} // namespace must

#endif /*TSANMESSAGES_H*/
