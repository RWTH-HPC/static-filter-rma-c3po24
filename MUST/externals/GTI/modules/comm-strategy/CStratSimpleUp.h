/* This file is part of GTI (Generic Tool Infrastructure)
 *
 * Copyright (C)
 *  2008-2019 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2013-2019 RWTH Aachen University, Federal Republic of Germany
 *
 * See the LICENSE file in the package base directory for details
 */

/**
 * @file CStratSimpleUp.h
 *        Simple implementation for the I_CommStrategyUp interface.
 *
 * Provides the interface functionality with a simple implementation.
 *
 * @author Tobias Hilbrich
 * @date 16.04.2009
 */

#include "GtiDefines.h"
#include "GtiEnums.h"
#include "GtiTypes.h"
#include "I_CommStrategyUp.h"
#include "I_CommProtocol.h"
#include "ModuleBase.h"
#include "CStratQueue.h"

#ifndef CSTRAT_SIMPLE_UP_H
#define CSTRAT_SIMPLE_UP_H

namespace gti
{
/**
 * Simple implementation of upwards strategy.
 * Properties:
 * - no aggregation
 * - uses ssend to send
 * - waits for an acknowledgment in the send
 */
class CStratSimpleUp : public ModuleBase<CStratSimpleUp, CStratUpQueue>
{
  protected:
    /** The communication protocol. */
    I_CommProtocol* protocol;

    unsigned int myRequest;
    bool reentrant{false};
    uint64_t myBuf[2];
    bool myGotPing;
    std::list<std::pair<uint64_t, uint64_t*>>
        myMsgQueue; /**< Queue of messages that where received when waiting for an acknowledgment*/

    static const uint64_t myTokenShutdownSync;
    static const uint64_t myTokenMessage;
    static const uint64_t myTokenAck;

  public:
    /**
     * Constructor.
     * @ref ModConf see for details on module configuration.
     * @param instanceName name of the module instance.
     */
    CStratSimpleUp(const char* instanceName);

    /**
     * Destructor.
     */
    ~CStratSimpleUp(void);

    /**
     * @see I_CommStrategyUp::getPlaceId
     */
    GTI_RETURN getPlaceId(uint64_t* outPlaceId);

    /**
     * @see I_CommStrategyUp::shutdown
     */
    GTI_RETURN shutdown(GTI_FLUSH_TYPE flush_behavior, GTI_SYNC_TYPE sync_behavior);

    /**
     * @see I_CommStrategyUp::send
     */
    GTI_RETURN send(
        void* buf,
        uint64_t num_bytes,
        void* free_data,
        GTI_RETURN (*buf_free_function)(void* free_data, uint64_t num_bytes, void* buf));

    /**
     * @see I_CommStrategyUp::test
     */
    GTI_RETURN test(
        int* out_flag,
        uint64_t* out_num_bytes,
        void** out_buf,
        void** out_free_data,
        GTI_RETURN (**out_buf_free_function)(void* free_data, uint64_t num_bytes, void* buf));

    /**
     * @see I_CommStrategyUp::wait
     */
    GTI_RETURN wait(
        uint64_t* out_num_bytes,
        void** out_buf,
        void** out_free_data,
        GTI_RETURN (**out_buf_free_function)(void* free_data, uint64_t num_bytes, void* buf));

    /**
     * @see I_CommStrategyUp::raisePanic
     */
    GTI_RETURN raisePanic(void);

    /**
     * @see I_PanicListener::flush
     */
    GTI_RETURN flush(void);

    /**
     * @see I_PanicListener::flushAndSetImmediate
     */
    GTI_RETURN flushAndSetImmediate(void);
};
} // namespace gti

#endif /* CSTRAT_SIMPLE_UP_H */
