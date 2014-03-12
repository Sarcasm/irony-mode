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
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <vector>

static void parseCommandLine(const std::vector<std::string> &argv) {
  for (std::vector<std::string>::const_iterator it = argv.begin() + 1,
                                                end = argv.end();
       it != end;
       ++it) {
    const std::string &arg = *it;

    if (arg == "--version") {
      // do not change the format for the first line, external programs should
      // be able to rely on it
      std::cout << "irony-mode version 0.1" << std::endl;
      exit(0);
    }

    if (arg == "--help" || arg == "-h") {
      std::cout << "usage: " << argv[0] << " [-h] [--version]\n";
      exit(0);
    } else {
      std::cerr << argv[0] << ": error: unrecognized argment '" << arg << "'\n";
      exit(1);
    }
  }
}

int main(int argc, const char *argv[]) {
  // Required for wstring
  setlocale(LC_CTYPE, "");

  parseCommandLine(std::vector<std::string>(argv, &argv[argc]));

  Server server;

  return server.run();
}
