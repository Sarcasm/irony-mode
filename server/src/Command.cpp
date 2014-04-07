#include "Command.h"

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>

namespace {

struct StringConverter {
  StringConverter(std::string *dest) : dest_(dest) {
  }

  bool operator()(const std::string &str) {
    *dest_ = str;
    return true;
  }

private:
  std::string *dest_;
};

struct UnsignedIntConverter {
  UnsignedIntConverter(unsigned *dest) : dest_(dest) {
  }

  bool operator()(const std::string &str) {
    try {
      int num = std::stoi(str);
      if (num > 0) {
        *dest_ = num;
        return true;
      }
    }
    catch (...) {
    }
    return false;
  }

private:
  unsigned *dest_;
};

} // unnamed namespace

std::ostream &operator<<(std::ostream &os, const Command::Action &action) {
  os << "Command::";

  switch (action) {
#define X(sym, str, help)                                                      \
  case Command::sym:                                                           \
    os << #sym;                                                                \
    break;
#include "Command.def"
  }
  return os;
}

std::ostream &operator<<(std::ostream &os, const Command &command) {
  os << "Command{action=" << command.action << ", "
     << "file='" << command.file << "', "
     << "line='" << command.line << "', "
     << "column='" << command.column << "', "
     << "flags=[";
  bool first = true;
  for (const std::string &flag : command.flags) {
    if (!first)
      os << ", ";
    os << flag;
    first = false;
  }
  os << "]";

  return os << "}";
}

static Command::Action actionFromString(const std::string &actionStr) {
#define X(sym, str, help)                                                      \
  if (actionStr == str)                                                        \
    return Command::sym;

#include "Command.def"

  return Command::Unknown;
}

Command *CommandParser::parse(const std::vector<std::string> &argv) {
  command_.clear();

  auto end = std::find(argv.begin(), argv.end(), "--");

  if (end != argv.end())
    command_.flags.assign(end + 1, argv.end());

  if (argv.begin() == end) {
    std::clog << "error: no command specified.\n"
                 "See 'irony-server help' to list available commands\n";
    return 0;
  }

  const std::string &actionStr = argv[0];

  command_.action = actionFromString(actionStr);

  std::vector<std::function<bool(const std::string &)>> positionalArgs;

  switch (command_.action) {
  case Command::CheckCompile:
    // file.cpp line column
    positionalArgs.push_back(StringConverter(&command_.file));
    positionalArgs.push_back(UnsignedIntConverter(&command_.line));
    positionalArgs.push_back(UnsignedIntConverter(&command_.column));
    break;

  case Command::Version:
  case Command::Help:
    break;

  case Command::Unknown:
    std::clog << "error: invalid command specified: " << actionStr << "\n";
    return 0;
  }

  auto argIt = argv.begin() + 1;
  int argCount = std::distance(argIt, end);

  if (argCount != static_cast<int>(positionalArgs.size())) {
    std::clog << "error: invalid number of arguments for '" << actionStr
              << "' (requires " << positionalArgs.size() << " got " << argCount
              << ")\n";
    return 0;
  }

  for (auto fn : positionalArgs) {
    if (!fn(*argIt)) {
      std::clog << "error: invalid argument " << *argIt << " for '" << actionStr
                << "\n";
      return 0;
    }
    ++argIt;
  }

  return &command_;
}
