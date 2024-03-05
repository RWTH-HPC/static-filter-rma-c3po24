/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "OpenMPsanity.h"

#include "BaseApi.h"
#include "GtiMacros.h"
#include "MustEnums.h"

#include <pnmpi/limit-threading.h>
#include <pnmpi/service.h>
#include <pnmpimod.h>

#include <atomic>
#include <memory>

using namespace gti;
using namespace must;
using namespace detail;

mGET_INSTANCE_FUNCTION(OpenMPsanity);
mFREE_INSTANCE_FUNCTION(OpenMPsanity);
mPNMPI_REGISTRATIONPOINT_FUNCTION(OpenMPsanity);

namespace must
{
namespace
{
/**
 * Represents a MPI thread level.
 */
enum class ThreadLevel {
    SINGLE = MPI_THREAD_SINGLE,
    FUNNELED = MPI_THREAD_FUNNELED,
    SERIALIZED = MPI_THREAD_SERIALIZED,
    MULTIPLE = MPI_THREAD_MULTIPLE,
    NONE ///< No thread level has been defined.
};

/**
 * Convert PnMPI's threading level to our own type.
 *
 * @param level the level to convert
 * @return the corresponding must::ThreadLevel
 */
constexpr auto to_ThreadingLevel(PNMPI_threading_level_t level) -> ThreadLevel
{
    return static_cast<ThreadLevel>(level);
}

class PnMPIError : public std::exception
{
    // Store the message behind shared_ptr to assert non-throwing copy.
    const std::shared_ptr<const std::string> myMsg{};

  public:
    PnMPIError(PNMPI_status_t status, const std::string& msg)
        : myMsg{std::make_shared<std::string>(msg + ": " + PNMPI_Service_strerror(status))}
    {
    }

    auto what() const noexcept -> const char* override { return myMsg->c_str(); }
};
static_assert(
    std::is_nothrow_copy_constructible<PnMPIError>(),
    "Exceptions are not permitted to throw exceptions during copy construction.");
static_assert(
    !std::is_assignable<PnMPIError, PnMPIError>() || std::is_nothrow_copy_assignable<PnMPIError>(),
    "Exceptions are not permitted to throw exceptions during copy assignment.");

/** Retrieve the thread level as required by the application.
 *
 * It returns the actually required value even when the PnMPI module "limit-threading" is used.
 *
 * \exception PnMPIError retrieving the PnMPI service or global failed.
 * \return The application's required thread level.
 */
auto getRequestedThreadLevel() -> ThreadLevel
{
    PNMPI_modHandle_t limit_threading_module = PNMPI_MODHANDLE_NULL;
    PNMPI_status_t res =
        PNMPI_Service_GetModuleByName(PNMPI_MODULE_LIMIT_THREADING, &limit_threading_module);
    if (res != PNMPI_SUCCESS) {
        throw PnMPIError{
            res,
            "Could not get the handle for the module \"" PNMPI_MODULE_LIMIT_THREADING "\""};
    }
    PNMPI_Global_descriptor_t required_level_desc;
    res = PNMPI_Service_GetGlobalByName(
        limit_threading_module,
        PNMPI_MODULE_LIMIT_THREADING_GLOBAL_REQUIRED,
        'i',
        &required_level_desc);
    if (res != PNMPI_SUCCESS) {
        throw PnMPIError{
            res,
            "Could not get the handle for the service global "
            "\"" PNMPI_MODULE_LIMIT_THREADING_GLOBAL_REQUIRED "\"."};
    }
    return to_ThreadingLevel(static_cast<PNMPI_threading_level_t>(*required_level_desc.addr.i));
}

/**
 * Base class that does not perform any check and always reports success.
 */
class CheckBase : public detail::I_SanityCheck
{
  public:
    auto notify_init_thread() -> GTI_ANALYSIS_RETURN override { return gti::GTI_ANALYSIS_SUCCESS; };
    auto notify_parallel_begin(MustParallelId, MustLocationId) -> GTI_ANALYSIS_RETURN override
    {
        return gti::GTI_ANALYSIS_SUCCESS;
    };
    auto enter_mpi_call(MustParallelId, MustLocationId) -> GTI_ANALYSIS_RETURN override
    {
        return gti::GTI_ANALYSIS_SUCCESS;
    };
    auto leave_mpi_call(MustParallelId, MustLocationId) -> GTI_ANALYSIS_RETURN override
    {
        return gti::GTI_ANALYSIS_SUCCESS;
    };

    virtual ~CheckBase() = default;
};

/**
 * Checks for correct usage of MPI_THREAD_SINGLE.
 */
class ThreadLevelSingleCheck final : public CheckBase
{
    I_CreateMessage& logger;

    auto notify_parallel_begin(MustParallelId pId, MustLocationId lId) noexcept
        -> GTI_ANALYSIS_RETURN override
    {
        logger.createMessage(
            MUST_ERROR_MPI_MULTIPLE_THREADS,
            pId,
            lId,
            MustErrorMessage,
            "Use of OpenMP parallel region prohibited: MPI initialized with 'MPI_THREAD_SINGLE'.");
        return GTI_ANALYSIS_FAILURE;
    }

  public:
    ThreadLevelSingleCheck(I_CreateMessage& logger) noexcept : logger{logger} {}
};

/**
 * Checks for correct usage of MPI_THREAD_FUNNELED.
 */
class ThreadLevelFunneledCheck final : public CheckBase
{
    I_CreateMessage& logger;
    bool mpi_main_thread = false;

    auto notify_init_thread() noexcept -> GTI_ANALYSIS_RETURN override
    {
        /* Set this instance as main thread.
         *
         * According to the MPI standard, MPI_Init_thread will be called at the
         * main thread only. For later checks if MPI calls are allowed on specific
         * threads, the status of the main thread will be saved in this instance
         * of the module.
         *
         * NOTE: This module does NOT check, whether MPI_Init_thread is called
         *       multiple times (e.g. by each of the individual threads), as this is
         *       checked by other modules. */
        mpi_main_thread = true;
        return gti::GTI_ANALYSIS_SUCCESS;
    }

    auto enter_mpi_call(MustParallelId pId, MustLocationId lId) -> GTI_ANALYSIS_RETURN override
    {
        if (!mpi_main_thread) {
            logger.createMessage(
                MUST_ERROR_MPI_MULTIPLE_THREADS,
                pId,
                lId,
                MustErrorMessage,
                "The MPI thread support level permits executing MPI calls on the "
                "main thread only.");
            return GTI_ANALYSIS_FAILURE;
        }
        return gti::GTI_ANALYSIS_SUCCESS;
    }

  public:
    ThreadLevelFunneledCheck(I_CreateMessage& logger) noexcept : logger{logger} {}
};

/**
 * Checks for correct usage of MPI_THREAD_SERIALIZED.
 *
 * Note: The current implementation is a naive one that actually checks for MPI calls happening in
 * parallel rather than happening concurrently. We might miss some errors right now.
 */
class ThreadLevelSerializedCheck final : public CheckBase
{
    I_CreateMessage& logger;
    static std::atomic<int> mpi_call_count; //< Keeps track of MPI calls that we're currently in.

    auto enter_mpi_call(MustParallelId pId, MustLocationId lId) -> GTI_ANALYSIS_RETURN override
    {
        /* For 'MPI_THREAD_SERIALIZED' MPI thread support level, the application
         * may perform MPI calls from different threads. However, the
         * application needs to take care about not executing MPI calls
         * concurrently by using locks or similar mechanisms. */
        if (mpi_call_count.fetch_add(1, std::memory_order_relaxed) + 1 != 1) {
            logger.createMessage(
                MUST_ERROR_MPI_MULTIPLE_THREADS,
                pId,
                lId,
                MustErrorMessage,
                "Multiple threads call MPI functions simultaneously while you "
                "required MPI_THREAD_SERIALIZED.");
            return gti::GTI_ANALYSIS_FAILURE;
        }
        return gti::GTI_ANALYSIS_SUCCESS;
    }

    auto leave_mpi_call(MustParallelId, MustLocationId) noexcept -> GTI_ANALYSIS_RETURN override
    {
        mpi_call_count.fetch_sub(1, std::memory_order_relaxed);
        return gti::GTI_ANALYSIS_SUCCESS;
    }

  public:
    ThreadLevelSerializedCheck(I_CreateMessage& logger) noexcept : logger{logger} {}
};
std::atomic<int> ThreadLevelSerializedCheck::mpi_call_count{0};
} // namespace

/* Constructor.
 *
 * For a detailed documentation see the related header file.
 */
OpenMPsanity::OpenMPsanity(const char* instanceName)
    : ModuleBase<OpenMPsanity, I_OpenMPsanity>(instanceName)
{
    /* Get submodules.
     *
     * A pointer to the dependent modules will be stored in member variables,
     * making them accessible for the lifetime of this object. */
    std::vector<I_Module*> subModInstances = createSubModuleInstances();
    logger = static_cast<I_CreateMessage*>(subModInstances[0]);

    try {
        switch (getRequestedThreadLevel()) {
        case ThreadLevel::SINGLE:
            myThreadLevelCheck =
                std::unique_ptr<I_SanityCheck>{new ThreadLevelSingleCheck{*logger}};
            break;
        case ThreadLevel::FUNNELED:
            myThreadLevelCheck =
                std::unique_ptr<I_SanityCheck>{new ThreadLevelFunneledCheck{*logger}};
            break;
        case ThreadLevel::SERIALIZED:
            myThreadLevelCheck =
                std::unique_ptr<I_SanityCheck>{new ThreadLevelSerializedCheck{*logger}};
            break;
        case ThreadLevel::NONE:
            logger->createMessage(
                MUST_MESSAGE_NO_ERROR,
                MustInformationMessage,
                "MPI has not been initialized yet. Checks of thread level usage will be disabled.");
        case ThreadLevel::MULTIPLE: // No checks needed for MPI_THREAD_MULTIPLE
            myThreadLevelCheck = std::unique_ptr<I_SanityCheck>{new CheckBase{}};
            break;
        }
    } catch (PnMPIError& e) {
        const auto info_message = std::string{"Could not determine the MPI thread level: "} +
                                  e.what() + "\nChecks of thread level usage will be disabled.";
        logger->createMessage(MUST_MESSAGE_NO_ERROR, MustInformationMessage, info_message);
        // Disable checks
        myThreadLevelCheck = std::unique_ptr<I_SanityCheck>{new CheckBase{}};
    }
}

/* Notify the checker about a threaded MPI initialization.
 *
 * For a detailed documentation see the related header file.
 */
GTI_ANALYSIS_RETURN OpenMPsanity::notifyThreadedMPI(int)
{
    return myThreadLevelCheck->notify_init_thread();
}

/* Notify the checker about the start of a parallel region.
 *
 * For a detailed documentation see the related header file.
 */
GTI_ANALYSIS_RETURN OpenMPsanity::notifyParallelBegin(MustParallelId pId, MustLocationId lId)
{
    return myThreadLevelCheck->notify_parallel_begin(pId, lId);
};

/* Notify the MPI thread support level checker before executing the MPI call.
 *
 * For a detailed documentation see the related header file.
 */
GTI_ANALYSIS_RETURN OpenMPsanity::enterMPICall(MustParallelId pId, MustLocationId lId)
{
    return myThreadLevelCheck->enter_mpi_call(pId, lId);
}

/* Notify the MPI thread support level checker after executing the MPI call.
 *
 * For a detailed documentation see the related header file.
 */
gti::GTI_ANALYSIS_RETURN OpenMPsanity::leaveMPICall(MustParallelId pId, MustLocationId lId)
{
    return myThreadLevelCheck->leave_mpi_call(pId, lId);
}

OpenMPsanity::~OpenMPsanity()
{
    if (logger != nullptr) {
        destroySubModuleInstance(static_cast<I_Module*>(logger));
        logger = nullptr;
    }
}

} // namespace must
