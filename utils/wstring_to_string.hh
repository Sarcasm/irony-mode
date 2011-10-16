/**
 * \file   wstring_to_string.hh
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Tue Jul 19 22:22:00 2011
 *
 * \brief  wstring_to_string header.
 *
 *
 */

#ifndef _IRONY_WSTRING_TO_STRING_HH_
#define _IRONY_WSTRING_TO_STRING_HH_

#include <string>

/**
 * Convert the \c std::wstring \p s into a \c std::string.
 *
 * \note An incorrect string can be generated when a character
 *       conversion fail.
 *
 * \note Global namespace pollution.
 *
 * \param s The wide string to convert.
 *
 * \return The converted string into a \c std::string.
 */
std::string     wstring_to_string(const std::wstring & s);

#endif /* !_IRONY_WSTRING_TO_STRING_HH_ */
