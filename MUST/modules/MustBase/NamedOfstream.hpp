/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file NamedOfstream.hpp
 */

#ifndef MUST_NAMEDOFSTREAM_HPP
#define MUST_NAMEDOFSTREAM_HPP

#include <fstream>
#include <string>

#include "MustDefines.h"
#include "MustOutputdir.h"

namespace must
{

/**
 * Wrapper class for custom ofstream.
 *
 * This class enhances the default @ref std::ofstream with the filename
 * of a specific stream, so it can be used from different positions in
 * the @ref MsgLoggerHtml class.
 */
class NamedOfstream : public std::ofstream
{
  public:
    /**
     * @see std::ofstream::ofstream
     */
    NamedOfstream() = default;

    /**
     * @see std::ofstream::ofstream
     *
     * In addition to opening a new @p filename, the filename will
     * be stored in  a private variable.
     */
    NamedOfstream(const std::string& filename)
        : std::ofstream(std::string{get_base_output_dir() + '/' + filename}), myFilename(filename)
    {
    }

    /**
     * @return The filename of this stream.
     */
    auto filename() const -> const std::string& { return myFilename; }

  private:
    /**
     * The filename of this stream.
     */
    std::string myFilename{};
};

} // namespace must

#endif // MUST_NAMEDOFSTREAM_HPP
