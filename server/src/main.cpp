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

#include "support/CommandLineParser.h"

#include <cassert>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <memory>
#include <vector>

static void printHelp() {
  std::cout
      << "usage: irony-server [--version|-v] [--help|-h] [--interactive|-i]\n"
         "                    <command> [<argv>]\n\n";

#define X(sym, str, desc)                                                      \
  if (Command::sym != Command::Unknown)                                        \
    std::cout << std::left << std::setw(18) << "  " str << desc << "\n";
#include "Commands.def"
}

static void printVersion() {
  // do not change the format for the first line, external programs should be
  // able to rely on it
  std::cout << "irony-server version 0.1.0\n";
}

struct CommandProviderInterface {
  virtual ~CommandProviderInterface() { }

  virtual std::vector<std::string> nextCommand() = 0;
};

struct CommandLineCommandProvider : CommandProviderInterface {
  CommandLineCommandProvider(const std::vector<std::string> &argv)
    : argv_(argv), firstCall_(true) {
  }

  std::vector<std::string> nextCommand() {
    if (firstCall_) {
      firstCall_ = false;
      return argv_;
    }

    return std::vector<std::string>(1, "exit");
  }

private:
  std::vector<std::string> argv_;
  bool firstCall_;
};

struct InteractiveCommandProvider : CommandProviderInterface {
  std::vector<std::string> nextCommand() {
    std::string line;

    if (std::getline(std::cin, line)) {
      return unescapeCommandLine(line);
    }

    return std::vector<std::string>(1, "exit");
  }
};

int main(int ac, const char *av[]) {
  std::vector<std::string> argv(&av[1], &av[ac]);

  bool interactiveMode = false;

  if (ac == 1) {
    printHelp();
    return 1;
  }

  if (argv[0][0] == '-') {
    if (argv[0] == "--help" || argv[0] == "-h") {
      printHelp();
      return 0;
    }

    if (argv[0] == "--version" || argv[0] == "-v") {
      printVersion();
      return 0;
    }

    if (argv[0] == "--interactive" || argv[0] == "-i") {
      interactiveMode = true;
    } else {
      std::cerr << "irony-server: '" << argv[0] << "' is not a valid command\n";
      return 1;
    }
  }

  Irony irony;
  CommandParser commandParser;
  std::unique_ptr<CommandProviderInterface> commandProvider;

  if (interactiveMode) {
    commandProvider.reset(new InteractiveCommandProvider());
  } else {
    commandProvider.reset(new CommandLineCommandProvider(argv));
  }

  while (Command *c = commandParser.parse(commandProvider->nextCommand())) {
    if (c->action != Command::Exit)
      std::clog << "execute: " << *c << "\n";

    switch (c->action) {
    case Command::Help:
      printHelp();
      break;

    case Command::CheckCompile:
      irony.check(c->file, c->flags, c->cxUnsavedFiles);
      break;

    case Command::Exit:
      return 0;

    case Command::Unknown:
      assert("shouldn't go here");
      break;
    }
    std::cout << "\n;;EOT\n";
  }

  return 1;
}
