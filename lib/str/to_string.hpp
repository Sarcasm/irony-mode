/**
 * \file   to_string.hpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Sat Jan 21 20:22:11 2012
 *
 * \brief  Convert a given value to it's string representation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_LIB_STR_TO_STRING_HPP_
#define IRONY_MODE_LIB_STR_TO_STRING_HPP_

#include <sstream>

namespace str {

template <class T> inline std::string to_string(const T & t)
{
  std::stringstream ss;
  ss << t;
  return ss.str();
}

template <> inline std::string to_string(const bool & b)
{
  return b ? "true" : "false";
}

} // ! namespace str

#endif /* !IRONY_MODE_LIB_STR_TO_STRING_HPP_ */
