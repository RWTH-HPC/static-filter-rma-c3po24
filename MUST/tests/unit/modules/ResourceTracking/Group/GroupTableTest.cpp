/*
 * Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file GroupTableTest.cpp
 *
 *  @date 04.06.23
 *  @author Sebastian Grabowski
 */

#include "GroupTable.h"

#include <memory>
#include <gtest/gtest.h>

namespace
{

namespace
{
class DestructableDeleter
{
  public:
    void operator()(must::I_Destructable* destructable) { destructable->erase(); }
};
} // namespace

template <typename T, typename... TArgs>
std::unique_ptr<T, DestructableDeleter> make_unique_destructable(TArgs&&... args)
{
    return std::unique_ptr<T, DestructableDeleter>{new T{std::forward<TArgs>(args)...}};
}

} // namespace

TEST(GroupTable, translateSetRepresentation)
{
    auto table = ::make_unique_destructable<must::GroupTable>(std::vector<int>{3, 5, 6}, nullptr);

    // Valid inputs
    int res = 0;
    ASSERT_TRUE(table->translate(0, &res));
    ASSERT_TRUE(res == 3);

    ASSERT_TRUE(table->translate(2, &res));
    ASSERT_TRUE(res == 6);

    // Invalid ranks should return false and leave res unchanged.
    res = -1337;
    ASSERT_FALSE(table->translate(-1, &res));
    ASSERT_EQ(res, -1337);

    ASSERT_FALSE(table->translate(3, &res));
    ASSERT_EQ(res, -1337);
}

TEST(GroupTable, translateRangeRepresentation)
{
    int const begin = 42;
    int const end = 46;
    auto table = ::make_unique_destructable<must::GroupTable>(begin, end, nullptr);

    // Valid inputs
    int res = 0;
    ASSERT_TRUE(table->translate(0, &res));
    ASSERT_EQ(res, 42);

    ASSERT_TRUE(table->translate(2, &res));
    ASSERT_EQ(res, 44);

    // Invalid ranks should return false and leave res unchanged.
    res = -1337;
    ASSERT_FALSE(table->translate(-1, &res));
    ASSERT_EQ(res, -1337);

    ASSERT_FALSE(table->translate(end - begin + 1, &res));
    ASSERT_EQ(res, -1337);
}
