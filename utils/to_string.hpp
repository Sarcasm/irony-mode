/**
 * @file   to_string.hpp
 * @author Guillaume Papin <guillaume.papin@epitech.eu>
 * @date   Thu Jun  2 14:24:05 2011
 *
 * @brief  Convert a given value to it's string representation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef _TO_STRING_HPP_
#define _TO_STRING_HPP_

#include <sstream>

template <class T>
inline std::string to_string(const T& t)
{
  std::stringstream ss;
  ss << t;
  return ss.str();
}

template <> inline std::string to_string(const bool & b)
{
  return b ? "true" : "false";
}

#endif /* _TO_STRING_HPP_ */
