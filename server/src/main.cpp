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
#include <iomanip>
#include <vector>

static void printHelp() {
  std::cout << "usage: irony-server [--version|-v] [--help|-h] [--interactive|-i]\n"
               "                    <command> [<args>]\n\n";

#define X(sym, str, desc)                                                      \
  if (Command::sym != Command::Unknown)                                        \
    std::cout << std::left << std::setw(18) << "  " str << desc << "\n";
#include "Commands.def"
}

static void printVersion() {
  // do not change the format for the first line, external programs should be
  // able to rely on it
  std::cout << "irony-mode version 0.1.0\n";
}

int main(int argc, const char *argv[]) {
  std::vector<std::string> args(&argv[1], &argv[argc]);

  Irony irony;
  CommandParser commandParser;
  bool InteractiveMode = false;

  if (argc == 1) {
    printHelp();
    return 1;
  }

  if (args[0][0] == '-') {
    if (args[0] == "--help" || args[0] == "-h") {
      printHelp();
      return 0;
    }

    if (args[0] == "--version" || args[0] == "-v") {
      printVersion();
      return 0;
    }

    if (args[0] == "--interactive" || args[0] == "-i") {
      InteractiveMode = true;
    } else {
      std::cerr << "irony-server: '" << args[0] << "' is not a valid command\n";
      return 1;
    }
  }

  if (InteractiveMode) {
    std::cout << "not yet supported..." << std::endl;
    return 1;
  }

  if (Command *c = commandParser.parse(args)) {
    std::clog << "execute: " << *c << "\n";

    switch (c->action) {
    case Command::Help:
      printHelp();
      break;

    case Command::CheckCompile:
      irony.check(c->file, c->flags);
      break;

    case Command::Unknown:
      break;
    }
  }

  return 0;
}
