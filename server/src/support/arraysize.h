/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief \c arraysize() and \c ARRAYSIZE_UNSAFE() definitions.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#ifndef IRONY_MODE_SERVER_SUPPORT_ARRAYSIZE_H_
#define IRONY_MODE_SERVER_SUPPORT_ARRAYSIZE_H_

#include <cstddef>

template <typename T, std::size_t N>
char (&ArraySizeHelper(T (&array)[N]))[N];

/// \brief Convenience macro to get the size of an array.
///
/// \note Found in an article about the Chromium project, see
/// http://software.intel.com/en-us/articles/pvs-studio-vs-chromium and
/// http://codesearch.google.com/codesearch/p?hl=en#OAMlx_jo-ck/src/base/basictypes.h&q=arraysize&exact_package=chromium
/// 
#define arraysize(array) (sizeof(ArraySizeHelper(array)))

/// \brief \c arraysize() 'unsafe' version to use when the \c arraysize() macro
/// can't be used.
/// 
/// The \c arraysize() macro can't be used on an anonymous structure array for
/// example.
/// 
/// \note Be careful to check that the array passed is not just a pointer.
/// 
#define ARRAYSIZE_UNSAFE(a)                                                    \
  ((sizeof(a) / sizeof(*(a))) /                                                \
   static_cast<size_t>(!(sizeof(a) % sizeof(*(a)))))

#endif /* !IRONY_MODE_SERVER_SUPPORT_ARRAYSIZE_H_ */
