/**-*-C++-*-
 * \file   Server.h
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Tue Jul 19 14:18:24 2011
 *
 * \brief  Server class definition (Unix Domain Socket).
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_SERVER_H_
#define IRONY_MODE_SERVER_SERVER_H_

#include "clang-c/Index.h"

#include "util/JSONObjectWrapper.h"
#include "util/NonCopyable.h"

#include "TUManager.h"
#include "IPlugin.h"

class Server : public util::NonCopyable
{
  typedef std::map<std::string, IPlugin *> PluginMap;

private:
  TUManager tuManager_;
  /**
   * \brief Map of plugins, the string is the "request" the plugin
   *        handles.
   */
  PluginMap plugins_;

public:
  Server();
  ~Server();

  /**
   * \brief Run the server, loop for input and process each command
   *        received in the standard input.
   *
   * \return 0 on sucess (End of transmission received), on error a
   *         number greater than 0.
   */
  int run();

private:

  /**
   * \brief Parse the JSON buffer \p buf and proceed to the request.
   *
   * \param buf A JSON string.
   */
  void parseJSONAndProceed(const std::string & buf);

/**
 * \brief Find a handler for the given \p request and process the
 *        request.
 *
 * \param request A string corresponding to a request (i.e.
 *                completion, error_checking, ...)
 * \param object The data object assotiated to the request.
 */
  void handleRequest(const std::string &       request,
                     const JSONObjectWrapper & json,
                     const JSONObjectWrapper & data);
};

#endif /* !IRONY_MODE_SERVER_SERVER_H_ */
