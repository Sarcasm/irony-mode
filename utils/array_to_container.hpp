/**
 * \file   array_to_container.hpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Wed Aug 31 13:44:53 2011
 *
 * \brief  make_container definition.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef _IRONY_ARRAY_TO_CONTAINER_HPP_
#define _IRONY_ARRAY_TO_CONTAINER_HPP_

template <typename T, std::size_t N>
class make_container_internal
{
private:
  T     (&array_)[N];

public:
  make_container_internal(T (&array)[N]) : array_(array) { }

  template <typename Container>
  operator Container()
  {
    return Container(array_, array_ + N);
  }
};

template <typename T, std::size_t N>
make_container_internal<T, N> make_container(T (&array)[N])
{
  return make_container_internal<T, N>(array);
}

#endif /* !_IRONY_ARRAY_TO_CONTAINER_HPP_ */
