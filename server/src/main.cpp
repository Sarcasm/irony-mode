/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#include "Irony.h"
#include "Command.h"

#include "support/CommandLineParser.h"

#include <cassert>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <memory>
#include <vector>

static void printHelp() {
  std::cout << "usage: irony-server [--version|-v] [--help|-h] "
               "[--interactive|-i] [--debug|-d]\n"
               "                    <command> [<argv>]\n\n";

#define X(sym, str, desc)                                                      \
  if (Command::sym != Command::Unknown)                                        \
    std::cout << std::left << std::setw(18) << "  " str << desc << "\n";
#include "Commands.def"
}

static void printVersion() {
  // do not change the format for the first line, external programs should be
  // able to rely on it
  std::cout << "irony-server version " IRONY_PACKAGE_VERSION "\n";
}

static void dumpUnsavedFiles(Command &command) {
  for (int i = 0; i < static_cast<int>(command.unsavedFiles.size()); ++i) {
    std::clog << "unsaved file " << i + 1 << ": "
              << command.unsavedFiles[i].first << "\n"
              << "----\n";
    std::copy(command.unsavedFiles[i].second.begin(),
              command.unsavedFiles[i].second.end(),
              std::ostream_iterator<char>(std::clog));
    std::clog << "----\n";
  }
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

  Irony irony;

  if (ac == 1) {
    printHelp();
    return 1;
  }

  int optCount = 0;

  for (const std::string &opt : argv) {
    if (opt[0] == '-') {
      ++optCount;

      if (opt == "--help" || opt == "-h") {
        printHelp();
        return 0;
      }

      if (opt == "--version" || opt == "-v") {
        printVersion();
        return 0;
      }

      if (opt == "--interactive" || opt == "-i") {
        interactiveMode = true;
      } else if (opt == "--debug" || opt == "-d") {
        irony.setDebug(true);
      } else {
        std::cerr << "error: invalid option '" << opt << "'\n";
        return 1;
      }
    }
  }

  argv.erase(argv.begin(), argv.begin() + optCount);

  CommandParser commandParser;
  std::unique_ptr<CommandProviderInterface> commandProvider;

  if (interactiveMode) {
    commandProvider.reset(new InteractiveCommandProvider());
  } else {
    commandProvider.reset(new CommandLineCommandProvider(argv));
  }

  while (Command *c = commandParser.parse(commandProvider->nextCommand())) {
    if (c->action != Command::Exit) {
      std::clog << "execute: " << *c << "\n";

      if (irony.isDebugEnabled()) {
        dumpUnsavedFiles(*c);
      }
    }

    switch (c->action) {
    case Command::Help:
      printHelp();
      break;

    case Command::CheckCompile:
      irony.check(c->file, c->flags, c->cxUnsavedFiles);
      break;

    case Command::Complete:
      irony.complete(c->file, c->line, c->column, c->flags, c->cxUnsavedFiles);
      break;

    case Command::Exit:
      return 0;

    case Command::SetDebug:
      irony.setDebug(c->opt);
      break;

    case Command::Unknown:
      assert(0 && "unreacheable code...reached!");
      break;
    }
    std::cout << "\n;;EOT\n";
  }

  return 1;
}
