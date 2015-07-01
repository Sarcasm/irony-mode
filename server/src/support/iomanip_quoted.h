/**-*-C++-*-
 * \file
 * \brief Dumb implementation of something that might look like C++14
 * std::quoted.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#ifndef IRONY_MODE_SERVER_SUPPORT_IOMANIP_QUOTED_H_
#define IRONY_MODE_SERVER_SUPPORT_IOMANIP_QUOTED_H_

#include <ostream>
#include <string>

namespace support {
namespace detail {

struct quoted_string_proxy {
  quoted_string_proxy(const std::string &s) : s(s) {
  }

  std::string s;
};

std::ostream &operator<<(std::ostream &os, const quoted_string_proxy &q) {
  const std::string &s = q.s;

  os << '"';
  if (s.find_first_of("\"\\") == std::string::npos) {
    os << s;
  } else {
    for (auto ch : s) {
      if (ch == '\\' || ch == '"')
        os << '\\';

      os << ch;
    }
  }
  os << '"';
  return os;
}

} // namespace detail

detail::quoted_string_proxy quoted(const std::string &s) {
  return detail::quoted_string_proxy(s);
}

} // namespace support

#endif // IRONY_MODE_SERVER_SUPPORT_IOMANIP_QUOTED_H_
