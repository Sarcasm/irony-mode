/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief wstring_to_string declaration.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_LIB_STR_WSTRING_TO_STRING_H_
#define IRONY_MODE_LIB_STR_WSTRING_TO_STRING_H_

#include <string>

namespace str {

/**
 * \brief Convert the \c std::wstring \p s into a \c std::string.
 *
 * \note An incorrect string can be generated when a character conversion fail.
 *
 * \param s
 *      The wide string to convert.
 *
 * \return The string \p s converted into a \c std::string.
 */
std::string wstring_to_string(const std::wstring &s);

} // ! namespace str

#endif /* !IRONY_MODE_LIB_STR_WSTRING_TO_STRING_H_ */
