/**
 * \file   IPlugin.hh
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Tue Jul 19 15:36:20 2011
 *
 * \brief  Plugin interface for CC Enhanced mode.
 *
 *
 */

#ifndef _CC_ENHANCED_IPLUGIN_HH_
#define _CC_ENHANCED_IPLUGIN_HH_

#include <string>

#include "TUManager.hh"

#include "JSONObjectWrapper.hh"

/**
 * A simple plugin interface.
 *
 */
class IPlugin
{
public:
  /**
   * Each plugin must implement this function, the string \p buf will
   * be sent to the Emacs process in response to the request.
   *
   * \param tuManager   The TUManager associated to the request.
   * \param data        The JSON object containing the plugin data.
   * \param [out] buf   The string to send to the server, this
   *                    argument should be filled by the function.
   */
  virtual std::string handleRequest(TUManager &               tuManager,
                                    const JSONObjectWrapper & data,
                                    std::string &             buf) = 0;

  virtual ~IPlugin() { }
};

/**
 * The plugin factory type.
 */
typedef IPlugin* (*iplugin_factory_t)();

#endif /* !_CC_ENHANCED_IPLUGIN_HH_ */
