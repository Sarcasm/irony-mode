/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief Plugin interface for the irony-mode server.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_IPLUGIN_H_
#define IRONY_MODE_SERVER_IPLUGIN_H_

#include "TUManager.h"

#include "util/JSONObjectWrapper.h"

#include <ostream>
#include <string>

/**
 * \brief A simple plugin interface.
 *
 * At the creation of a plugin the server TUManager is given and the plugin can
 * configure it to suit its needs (e.g: activate some special flags for Clang)
 * and can it can also be used in the \c handleRequest() method when working on
 * CXTranslationUnit (and take advantage of it's caching).
 *
 * \note If a plugin add some configurations to the TUManager at construction
 *       time it should clean them up in the destructor.
 */
class IPlugin {
public:
  /**
   * \brief Each plugin must implement this function, the string \p buf will be
   *        sent to the Emacs process in response to the request.
   *
   * \param data
   *            The JSON object containing the plugin data.
   * \param [out] buf
   *            The string to send to the server, this argument should be filled
   *            by the function.
   *
   * \return The answer "type" to the request.
   */
  virtual std::string handleRequest(const JSONObjectWrapper &data,
                                    std::ostream &out) = 0;

  virtual ~IPlugin();
};

/**
 * \brief The plugin factory type.
 */
typedef IPlugin *(*iplugin_factory_t)();

#endif /* !IRONY_MODE_SERVER_IPLUGIN_H_ */
