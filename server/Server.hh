/**
 * \file   Server.hh
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Tue Jul 19 14:18:24 2011
 *
 * \brief  Server class definition (Unix Domain Socket).
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef _IRONY_SERVER_SERVER_HH_
#define _IRONY_SERVER_SERVER_HH_

#include "clang-c/Index.h"

#include "JSONObjectWrapper.hh"

#include "TUManager.hh"
#include "IPlugin.hh"

class Server
{
private:
  TUManager                        TUManager_; /**< Translation unit
                                                  manager */
  std::map<std::string, IPlugin *> plugins_;  /**< map of plugins, the
                                                 string is the
                                                 "request" the plugin
                                                 handle */

public:
  Server();
  ~Server();

  /**
   * Run the server, loop for input and process each command received
   * in the standard input.
   *
   * \return 0 on sucess (End of transmission received), on error a
   *         number greater than 0.
   */
  int run();

private:

  /**
   * Parse the JSON buffer \p buf and proceed to the request.
   *
   * \param buf A JSON string.
   */
  void parseJSONAndProceed(const std::string & buf);

/**
 * Find a handler for the given \p request and process the request.
 *
 * \param request A string corresponding to a request (i.e.
 *                completion, error_checking, ...)
 * \param object The data object assotiated to the request.
 */
  void handleRequest(const std::string &       request,
                     const JSONObjectWrapper & json,
                     const JSONObjectWrapper & data);

private:
  Server(Server const &);
  Server& operator=(Server const &);
};

#endif /* !_IRONY_SERVER_SERVER_HH_ */
