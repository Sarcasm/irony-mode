/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "Irony.h"
#include "Command.h"

#include <cstdlib>
#include <iostream>
#include <vector>

static void printHelp(const std::string &programName) {
  std::cout << "usage: " << programName << " [-h] [--version]\n";
}

static void parseGlobalOptions(const std::string &programName,
                               std::vector<std::string> &argv) {
  std::vector<std::string>::const_iterator it = argv.begin();

  for (; it != argv.end(); ++it) {
    const std::string &arg = *it;

    if (arg == "--version") {
      // do not change the format for the first line, external programs should
      // be able to rely on it
      std::cout << "irony-mode version 0.1.0\n";
      exit(0);
    }

    if (arg == "--help" || arg == "-h") {
      printHelp(programName);
      exit(0);
    } else if (!arg.empty() && arg[0] == '-') {
      std::cerr << programName << ": error: unrecognized program option '"
                << arg << "'\n";
      exit(1);
    } else {
      return;
    }
  }
}

int main(int argc, const char *argv[]) {
  std::string programName = argv[0];
  std::vector<std::string> args(&argv[1], &argv[argc]);

  // XXX: exits if any global option is provided, this behavior may be changed
  // as global option are added.
  parseGlobalOptions(programName, args);

  Irony irony;
  CommandParser commandParser;

  if (Command *c = commandParser.parse(args)) {
    std::clog << "execute: " << *c << "\n";

    switch (c->action) {
    case Command::CheckCompile:
      irony.check(c->file, c->flags);
      break;
    default:
      // cerr << "unsupported"
      break;
    }
  }

  return 0;
}
