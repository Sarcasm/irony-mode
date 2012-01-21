/**
 * \file   IPlugin.h
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Tue Jul 19 15:36:20 2011
 *
 * \brief  Plugin interface for the irony-mode server.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_IPLUGIN_H_
#define IRONY_MODE_SERVER_IPLUGIN_H_

#include <string>

#include "TUManager.h"

#include "util/JSONObjectWrapper.h"

/**
 * \brief A simple plugin interface.
 *
 */
class IPlugin
{
public:
  /**
   * \brief Each plugin must implement this function, the string
   *        \p buf will be sent to the Emacs process in response to
   *        the request.
   *
   * \param tuManager
   *            The TUManager associated to the request.
   * \param data
   *            The JSON object containing the plugin data.
   * \param [out] buf
   *            The string to send to the server, this argument should
   *            be filled by the function.
   *
   * \return The answer "type" to the request.
   */
  virtual std::string handleRequest(TUManager &               tuManager,
                                    const JSONObjectWrapper & data,
                                    std::string &             buf) = 0;

  virtual ~IPlugin() { }
};

/**
 * \brief The plugin factory type.
 */
typedef IPlugin* (*iplugin_factory_t)();

#endif /* !IRONY_MODE_SERVER_IPLUGIN_H_ */
