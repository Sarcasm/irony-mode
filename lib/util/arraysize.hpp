/**
 * \file   arraysize.hpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Tue Jul 19 19:12:09 2011
 *
 * \brief  arraysize() and ARRAYSIZE_UNSAFE() definitions.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_LIB_UTIL_ARRAYSIZE_HPP_
#define IRONY_MODE_LIB_UTIL_ARRAYSIZE_HPP_

#include <cstddef>

template <typename T, std::size_t N>
char (&ArraySizeHelper(T (&array)[N]))[N];

/**
 * \brief Nice arraysize calculation found in an article about the
 *        Chromium project.
 * \see http://software.intel.com/en-us/articles/pvs-studio-vs-chromium
 * \see http://codesearch.google.com/codesearch/p?hl=en#OAMlx_jo-ck/src/base/basictypes.h&q=arraysize&exact_package=chromium
 *
 */
#define arraysize(array) (sizeof(ArraySizeHelper(array)))

/**
 * \brief arraysize() 'unsafe' version to use when the arraysize()
 *        macro can't be used.
 *
 * arraysize() macro can't be used on an anonymous structure array for
 * example.
 *
 * \note Check if the array is not a pointer.
 *
 */
#define ARRAYSIZE_UNSAFE(a)                             \
  ((sizeof(a) / sizeof(*(a))) /                         \
   static_cast<size_t>(!(sizeof(a) % sizeof(*(a)))))

#endif /* !IRONY_MODE_LIB_UTIL_ARRAYSIZE_HPP_ */
