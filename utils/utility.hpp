/**
 * \file   utility.hpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Tue Jul 19 19:12:09 2011
 *
 * \brief  Usefull stuff.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef _IRONY_UTILITY_HPP_
#define _IRONY_UTILITY_HPP_

#include <cstddef>

// Nice arraysize calculation found in an article about the Chromium
// project.
// \see http://software.intel.com/en-us/articles/pvs-studio-vs-chromium
// \see http://codesearch.google.com/codesearch/p?hl=en#OAMlx_jo-ck/src/base/basictypes.h&q=arraysize&exact_package=chromium
template <typename T, size_t N>
char (&ArraySizeHelper(T (&array)[N]))[N];
#define arraysize(array) (sizeof(ArraySizeHelper(array)))

// When an anonymous structure array is used for example.
#define ARRAYSIZE_UNSAFE(a)                             \
  ((sizeof(a) / sizeof(*(a))) /                         \
   static_cast<size_t>(!(sizeof(a) % sizeof(*(a)))))

#endif /* !_IRONY_UTILITY_HPP_ */
