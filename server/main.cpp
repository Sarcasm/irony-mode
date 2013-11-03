/**
 * \file   main.cpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Wed Jul 13 18:21:04 2011
 *
 * \brief  File containing the main() function (application entry
 *         point).
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include <clocale>
#include <iomanip>
#include <iostream>

#include "Server.h"

/**
 * \brief Entry point of the server, options are available mainly for
 *        debug.
 *
 * \param argc
 *              Number of arguments in the command line.
 * \param argv
 *              Command line arguments.
 *
 * \return 0 on success 1 on failure.
 */
int	main()
{
  // Required for wstring
  setlocale(LC_CTYPE, "");

  Server server;

  return server.run();
}
