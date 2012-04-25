/**
 * \file   Server.cpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Tue Jul 19 15:29:55 2011
 *
 * \brief  Server implementation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "Server.h"

#include <iostream>
#include <string>
#include <unistd.h>

#include <string>
#include <cstdio>
#include <cerrno>

#include "util/arraysize.hpp"
#include "str/wstring_to_string.h"

// List of plugins
#include "plugins/CodeCompletion.h"
#include "plugins/SyntaxChecker.h"

namespace {
/// An empty line with EOT
const std::string eot_str               = "\nEOT\n";
/// Initial size of the completion buffer
const std::size_t SEXP_BUF_INITIAL_SIZE = 16384;

const std::map<std::string, IPlugin *> generateBundlePlugins()
{
  const std::pair<std::string, IPlugin*> plugins[] =
    {
      std::make_pair("complete", new CodeCompletion(true)),
      std::make_pair("complete-simple", new CodeCompletion(false)),
      std::make_pair("syntax-check", new SyntaxChecker())
    };
  return std::map<std::string, IPlugin *>(plugins, plugins + arraysize(plugins));
}
} // !namespace

Server::Server()
  : plugins_(generateBundlePlugins())
{ }

Server::~Server()
{ }

int Server::run()
{
  char        buf[16384];
  std::string sbuf;

  sbuf.reserve(arraysize(buf));

  while (true)
    {
      ssize_t nb_read = ::read(0, buf, arraysize(buf));

      if (nb_read == -1)
        {
          std::clog << strerror(errno) << std::endl;
          return 1;
        }
      if (nb_read == 0)
        return 0;

      buf[nb_read] = '\0';
      sbuf += buf;

      std::size_t start = 0;
      std::size_t found;

      while ((found = sbuf.find(eot_str, start)) != std::string::npos)
        {
          parseJSONAndProceed(sbuf.substr(start, found - start));
          start = found + eot_str.length();
        }

      if (start != 0)
        sbuf.erase(0, start + eot_str.length());
    }
}

void Server::parseJSONAndProceed(const std::string & buf)
{
  JSONObjectWrapper json(JSON::Parse(buf.c_str()));

  if (json)
    {
      bool                    valid   = true;
      const std::string       request = json.check(L"request", valid);
      const JSONObjectWrapper data(json.check(L"data", valid));

      if (valid)
        {
          handleRequest(request, json, data);
        }
      else
        {
          std::clog << "-- 8< --- invalid request received -----------\n"
                    << buf
                    << "\n------------- 8< -----------------------------\n";
        }
    }
}

void Server::handleRequest(const std::string &       request,
                           const JSONObjectWrapper & json,
                           const JSONObjectWrapper & data)
{
  std::string         intro("(");
  std::string         type("nil");
  std::string         buf;
  bool                hasBuffer = true;
  const std::string & buffer    = json.check(L"buffer", hasBuffer);

  if (hasBuffer)
    intro.append(":buffer \"").append(buffer).append("\" ");

  if (IPlugin *current = plugins_[request])
    {
      // Try to minimise the cost of buf += / buf.append() done by the
      // handleRequest call.
      buf.reserve(SEXP_BUF_INITIAL_SIZE);

      // Fill \c buf with the request answer.
      type = current->handleRequest(TUManager_, data, buf);
    }

  std::cout << intro << ":type " << type << " " << buf << ")\n;;EOT" << std::endl;
}
