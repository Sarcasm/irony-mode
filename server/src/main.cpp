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

int main(int argc, const char *argv[]) {
  std::string programName = argv[0];
  std::vector<std::string> args(&argv[1], &argv[argc]);

  Irony irony;
  CommandParser commandParser;

  if (Command *c = commandParser.parse(args)) {
    std::clog << "execute: " << *c << "\n";

    switch (c->action) {
    case Command::CheckCompile:
      irony.check(c->file, c->flags);
      break;

    case Command::Version:
      // do not change the format for the first line, external programs should
      // be able to rely on it
      std::cout << "irony-mode version 0.1.0\n";
      break;

    case Command::Help:
      printHelp(programName);
      break;

    case Command::Unknown:
      // cerr << "unsupported"
      break;
    }
  }

  return 0;
}
