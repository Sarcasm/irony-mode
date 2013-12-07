/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "Server.h"

#include <clocale>
#include <iomanip>
#include <iostream>

int main() {
  // Required for wstring
  setlocale(LC_CTYPE, "");

  Server server;

  return server.run();
}
