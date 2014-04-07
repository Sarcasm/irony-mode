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

static void printHelp(const std::string &programName) {
  std::cout << "usage: " << programName << " <command> [<args>]\n\n";

#define X(sym, str, desc)                                                      \
  if (Command::sym != Command::Unknown)                                        \
    std::cout << std::left << std::setw(18) << "  " str << desc << "\n";
#include "Command.def"
}

int main(int argc, const char *argv[]) {
  std::string programName = argv[0];
  std::vector<std::string> args(&argv[1], &argv[argc]);

  Irony irony;
  CommandParser commandParser;

  if (Command *c = commandParser.parse(args)) {
    std::clog << "execute: " << *c << "\n";

    switch (c->action) {
    case Command::Help:
      printHelp(programName);
      break;

    case Command::Version:
      // do not change the format for the first line, external programs should
      // be able to rely on it
      std::cout << "irony-mode version 0.1.0\n";
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
