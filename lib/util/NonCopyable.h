/**
 * \file   NonCopyable.h
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Sat Jan 21 20:03:20 2012
 *
 * \brief  NonCopyable class like in Boost.
 *
 * \see    http://en.wikibooks.org/wiki/More_C%2B%2B_Idioms/Non-copyable_Mixin
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_LIB_UTIL_NONCOPYABLE_HPP_
#define IRONY_MODE_LIB_UTIL_NONCOPYABLE_HPP_

namespace util {

class NonCopyable
{
protected:
  NonCopyable()
  { }

  // Protected non-virtual destructor
  ~NonCopyable()
  { }

private:
  NonCopyable(const NonCopyable &);
  NonCopyable & operator=(const NonCopyable &);
};

} // ! namespace util

#endif /* !IRONY_MODE_LIB_UTIL_NONCOPYABLE_HPP_ */
