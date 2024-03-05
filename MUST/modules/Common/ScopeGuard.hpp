/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MUST_SCOPEGUARD_HPP
#define MUST_SCOPEGUARD_HPP

#include <type_traits> // for static assertions
#include <functional>  // for std::function
#include <utility>     // std::move
#ifdef __cpp_lib_concepts
#include <concepts> // std::invocable
#endif

/**
 * RAII helper that executes a function object on destruction.
 *
 * @tparam FunctionType must be noexcept invocable without arguments
 */
template <typename FunctionType>
#if defined(__cpp_lib_concepts)
requires std::invocable<FunctionType>
#endif
class ScopeGuard
{
    FunctionType myCleanup;

  public:
    ScopeGuard() = delete;

    /**
     * @param cleanup function object that will be called on destruction. The signature of the
     *        function should be equivalent to the following: `void f()`.
     */
    explicit ScopeGuard(const FunctionType& cleanup) : myCleanup{cleanup} {}

    /**
     * @copydoc Scopeguard(const FunctionType&)
     */
    explicit ScopeGuard(FunctionType&& cleanup) : myCleanup{std::move(cleanup)} {}

    ScopeGuard(const ScopeGuard&) = delete;
    auto operator=(const ScopeGuard&) -> ScopeGuard& = delete;

    ScopeGuard(ScopeGuard&& other) noexcept = default;
    auto operator=(ScopeGuard&& other) noexcept -> ScopeGuard& = default;

    ~ScopeGuard() { myCleanup(); }
};

static_assert(!std::is_copy_assignable<ScopeGuard<std::function<void()>>>::value, "");
static_assert(!std::is_copy_constructible<ScopeGuard<std::function<void()>>>::value, "");
static_assert(std::is_move_assignable<ScopeGuard<std::function<void()>>>::value, "");
static_assert(std::is_move_constructible<ScopeGuard<std::function<void()>>>::value, "");

/* Prevents hard to find bugs like this:
 * ```cpp
 * void f() {
 *   // This will not call foo but just declare a ScopeGuard named foo.
 *   ScopeGuard(foo);
 * }
 * ```
 */
static_assert(
    !std::is_default_constructible<ScopeGuard<std::function<void()>>>::value,
    "Default constructible RAII guards are bugprone due to C++ syntax specialties.");

#endif // MUST_SCOPEGUARD_HPP
