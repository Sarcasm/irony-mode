/**
 * \file   utility.hpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Tue Jul 19 19:12:09 2011
 *
 * \brief  Usefull stuff.
 *
 *
 */

#ifndef _IRONY_UTILITY_HPP_
#define _IRONY_UTILITY_HPP_

// Nice arraysize calculation found in an article about the Chromium
// project.
// \see http://software.intel.com/en-us/articles/pvs-studio-vs-chromium
template <typename T, size_t N>
char (&ArraySizeHelper(T (&array)[N]))[N];
#define arraysize(array) (sizeof(ArraySizeHelper(array)))

#endif /* !_IRONY_UTILITY_HPP_ */
