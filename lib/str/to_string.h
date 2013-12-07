/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief Convert a given value to it's string representation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_LIB_STR_TO_STRING_H_
#define IRONY_MODE_LIB_STR_TO_STRING_H_

#include <sstream>

namespace str {

template <class T>
inline std::string to_string(const T &t) {
  std::stringstream ss;
  ss << t;
  return ss.str();
}

template <>
inline std::string to_string(const bool &b) {
  return b ? "true" : "false";
}

} // ! namespace str

#endif /* !IRONY_MODE_LIB_STR_TO_STRING_H_ */
