/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief wstring_to_string implementation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "str/wstring_to_string.h"

#include <cstring> // memset()
#include <cwchar>  // wcs*()
#include <vector>

namespace str {

std::string wstring_to_string(const std::wstring &s) {
  const wchar_t *wcs = s.c_str();
  std::size_t len = std::wcsrtombs(NULL, &wcs, 0, NULL);

  if (len != (std::size_t) - 1) {
    std::vector<char> dest(len);
    std::mbstate_t state;

    std::memset(&state, 0, sizeof state);
    wcs = s.c_str();
    len = std::wcsrtombs(&dest[0], &wcs, len, &state);

    if (len != (std::size_t) - 1) {
      return std::string(dest.begin(), dest.end());
    }
  }
  // conversion error try something stupid
  return std::string(s.begin(), s.end());
}

} // ! namespace str
