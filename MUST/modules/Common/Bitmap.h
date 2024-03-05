/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file Bitmap.h
 *
 *  @date 12.10.2022
 *  @author Niko Sakic
 */

#include <atomic>
#include <cstddef>
#include <cstdint>
#include <sys/types.h>
#include <vector>

#pragma once

namespace must
{

/**
 * Class uses vector<atomic<uint8_t>> to store values in a bitmap.
 */
class Bitmap
{
  private:
    std::vector<std::atomic<uint8_t>> map;
    size_t bits;

  public:
    /** Constructor
     *   initialises map and sets all values to zero.
     *   @param size
     */
    Bitmap(size_t size)
    {
        this->map = std::vector<std::atomic<uint8_t>>((size + 7) / 8);
        this->bits = size;

        for (auto& x : this->map) {
            x = 0;
        }
    }

    /**
     * Copy Constructor, uses Constructor to init the map and copies values.
     * @param other Bitmap to be copied
     */
    Bitmap(const Bitmap& other) : Bitmap(other.bits)
    {
        for (size_t i = 0; i < this->map.size(); ++i) {
            this->map[i] = other.map[i].load();
        }
    }

    /** Copy Assign Constructor
     * creates map of equal size and copies values from other.map
     * @param other Bitmap to be copied.
     */
    Bitmap& operator=(const Bitmap& other)
    {
        this->map = std::vector<std::atomic<uint8_t>>(other.map.size());
        this->bits = other.bits;
        for (size_t i = 0; i < this->map.size(); ++i) {
            this->map[i] = other.map[i].load();
        }
        return *this;
    }

    /** sets bit at given position to one.
     *   @param x index of bits.
     */
    void set(size_t x) { this->map[x / 8] |= 1 << (x % 8); }

    /** sets bits within range to one.
     * @param start first bit in range.
     * @param end last bit in range.
     */
    void set_range(size_t start, size_t end)
    {
        for (size_t i = start; i <= end; ++i) {
            if (i % 8 == 0 && i + 7 <= end) {
                this->map[i / 8] = 0xff;
                i += 7;
                continue;
            }
            this->map[i / 8] |= 1 << (i % 8);
        }
    }

    // reverts all bits in map back to zero.
    void clearAll()
    {
        for (auto& x : this->map) {
            x = 0;
        }
    }

    /** returns size of the bitmap.
     * @return size_t number of bits.
     */
    size_t size() const { return this->bits; }

    /** returns value of bit at given position.
     *   @param x index of bit.
     *   @return bool value of bit x.
     */
    bool get(size_t x) const { return map[x / 8] & (1 << (x % 8)); }

    /** Returns number of bits with value 1.
     * @return size_t count
     */
    size_t count()
    {
        size_t x = 0;
        for (size_t i = 0; i < bits; ++i) {
            x += get(i);
        }
        return x;
    }

    /** returns bool value of bit at given index.
     *   @param i index of bit.
     *   @return bool value of bit i.
     */
    bool operator[](size_t i) const { return get(i); }
};
} // namespace must
