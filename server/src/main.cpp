/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#include "Irony.h"
#include "Command.h"

#include "support/CIndex.h"
#include "support/CommandLineParser.h"

#include <cassert>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <memory>
#include <vector>

static void printHelp() {
  std::cout << "usage: irony-server [OPTIONS...] [COMMAND] [ARGS...]\n"
    "\n"
    "Options:\n"
    "  -v, --version\n"
    "  -h, --help\n"
    "  -i, --interactive\n"
    "  -d, --debug\n"
    "  --log-file PATH\n"
    "\n"
    "Commands:\n";

#define X(sym, str, desc)                                                      \
  if (Command::sym != Command::Unknown)                                        \
    std::cout << std::left << std::setw(25) << "  " str << desc << "\n";
#include "Commands.def"
}

static void printVersion() {
  // do not change the format for the first line, external programs should be
  // able to rely on it
  std::cout << "irony-server version " IRONY_PACKAGE_VERSION "\n";

  CXString cxVersionString = clang_getClangVersion();
  std::cout << clang_getCString(cxVersionString) << "\n";
  clang_disposeString(cxVersionString);
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

struct RestoreClogOnExit {
  RestoreClogOnExit() : rdbuf_(std::clog.rdbuf()) {
  }

  ~RestoreClogOnExit() {
    std::clog.rdbuf(rdbuf_);
  }

private:
  RestoreClogOnExit(const RestoreClogOnExit &);
  RestoreClogOnExit &operator=(const RestoreClogOnExit &);

private:
  std::streambuf *rdbuf_;
};

int main(int ac, const char *av[]) {
  std::vector<std::string> argv(&av[1], &av[ac]);

  // stick to STL streams, no mix of C and C++ for IO operations
  std::ios_base::sync_with_stdio(false);

  bool interactiveMode = false;

  Irony irony;

  if (ac == 1) {
    printHelp();
    return 1;
  }

  std::ofstream logFile;

  // When logging to a specific file, std::clog.rdbuf() is replaced by the log
  // file's one. When we return from the main, this buffer is deleted (at the
  // same time as logFile) but std::clog is still active, and will try to
  // release the rdbuf() which has already been released in logFile's
  // destructor. To avoid this we restore std::clog()'s original rdbuf on exit.
  RestoreClogOnExit clogBufferRestorer;

  unsigned optCount = 0;
  while (optCount < argv.size()) {
    const std::string &opt = argv[optCount];

    if (opt.c_str()[0] != '-')
      break;

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
    } else if (opt == "--log-file" && (optCount + 1) < argv.size()) {
      ++optCount;
      logFile.open(argv[optCount]);
      std::clog.rdbuf(logFile.rdbuf());
    } else {
      std::cerr << "error: invalid option '" << opt << "'\n";
      return 1;
    }

    ++optCount;
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
      std::clog << "execute: " << *c << std::endl;
    }

    switch (c->action) {
    case Command::Help:
      printHelp();
      break;

    case Command::Candidates:
      irony.candidates(c->prefix, c->style);
      break;

    case Command::CompletionDiagnostics:
      irony.completionDiagnostics();
      break;

    case Command::Complete:
      irony.complete(c->file, c->line, c->column, c->flags);
      break;

    case Command::Diagnostics:
      irony.diagnostics();
      break;

    case Command::Exit:
      return 0;

    case Command::GetCompileOptions:
      irony.getCompileOptions(c->dir, c->file);
      break;

    case Command::GetType:
      irony.getType(c->line, c->column);
      break;

    case Command::Parse:
      irony.parse(c->file, c->flags);
      break;

    case Command::SetDebug:
      irony.setDebug(c->opt);
      break;

    case Command::SetUnsaved:
      irony.setUnsaved(c->file, c->unsavedFile);
      break;

    case Command::ResetUnsaved:
      irony.resetUnsaved(c->file);
      break;

    case Command::Unknown:
      assert(0 && "unreacheable code...reached!");
      break;
    }

    std::cout << "\n;;EOT\n" << std::flush;
  }

  return 1;
}
