/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief Reload flag/Cache invalidation plugin class declaration.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_PLUGINS_CACHEINVALIDATION_H_
#define IRONY_MODE_SERVER_PLUGINS_CACHEINVALIDATION_H_

#include "IPlugin.h"

/**
 * A plugin that delete from TU Manager the cached flags on a given file.
 *
 */
class CacheInvalidation : public IPlugin {
public:
  CacheInvalidation(TUManager &tuManager);
  virtual ~CacheInvalidation();

  /**
   * \brief Execute a cache invalidation request for the given file.
   *
   * \sa IPlugin
   */
  virtual std::string handleRequest(const JSONObjectWrapper &data,
                                    std::ostream &out);

private:
  TUManager &tuManager_;
};

#endif /* !IRONY_MODE_SERVER_PLUGINS_CACHEINVALIDATION_H_ */
