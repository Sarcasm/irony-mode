/**-*-C++-*-
 * \file   CacheInvalidation.h
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Fri Jan 11 18:01:07 2013
 *
 * \brief  Reload flag/Cache invalidation plugin class declaration.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef CACHEINVALIDATION_H_
#define CACHEINVALIDATION_H_

#include "IPlugin.h"

/**
 * \brief A plugin that delete from TU Manager the cached flags on a
 *        given file.
 *
 */
class CacheInvalidation : public IPlugin
{
private:
  TUManager & tuManager_;

public:
  CacheInvalidation(TUManager & tuManager);
  virtual ~CacheInvalidation();

  /**
   * \brief Execute a cache invalidation request for the given file.
   *
   * \sa IPlugin
   */
  virtual std::string handleRequest(const JSONObjectWrapper & data,
                                    std::string &             buf);
};

#endif /* !CACHEINVALIDATION_H_ */
