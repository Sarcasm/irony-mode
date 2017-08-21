/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief Command parser definitions.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#include "Command.h"

#include "support/CommandLineParser.h"

#include <algorithm>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <map>


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
    char *end;
    long num = std::strtol(str.c_str(), &end, 10);

    if (end != (str.c_str() + str.size()))
      return false;

    if (errno == ERANGE)
      return false;

    if (num < 0)
      return false;

    unsigned long unum = static_cast<unsigned long>(num);
    if (unum > std::numeric_limits<unsigned>::max())
      return false;

    *dest_ = unum;
    return true;
  }

private:
  unsigned *dest_;
};

/// Convert "on" and "off" to a boolean
struct OptionConverter {
  OptionConverter(bool *dest) : dest_(dest) {
  }

  bool operator()(const std::string &str) {
    if (str == "on") {
      *dest_ = true;
    } else if (str == "off") {
      *dest_ = false;
    } else {
      return false;
    }
    return true;
  }

private:
  bool *dest_;
};

const std::map<std::string, PrefixMatchStyle> PREFIX_MATCH_STYLE_MAP = {
  { "exact", PrefixMatchStyle::Exact },
  { "case-insensitive", PrefixMatchStyle::CaseInsensitive },
  { "smart-case", PrefixMatchStyle::SmartCase},
};

/// Convert style to a PrefixMatchStyle
struct PrefixMatchStyleConverter {
  PrefixMatchStyleConverter(PrefixMatchStyle *dest) : dest_(dest) {
  }

  bool operator()(const std::string &str) {
    auto res = PREFIX_MATCH_STYLE_MAP.find(str);

    if (res == PREFIX_MATCH_STYLE_MAP.cend()) {
      return false;
    }
    *dest_ = res->second;
    return true;
  }

private:
  PrefixMatchStyle *dest_;
};

std::ostream &operator<<(std::ostream &os, PrefixMatchStyle style) {
  for (auto it : PREFIX_MATCH_STYLE_MAP) {
    if (it.second == style) {
      os << it.first;
      return os;
    }
  }
  os << "UnknownStyle";
  return os;
}


} // unnamed namespace

std::ostream &operator<<(std::ostream &os, const Command::Action &action) {
  os << "Command::";

  switch (action) {
#define X(sym, str, help)                                                      \
  case Command::sym:                                                           \
    os << #sym;                                                                \
    break;
#include "Commands.def"
  }
  return os;
}

std::ostream &operator<<(std::ostream &os, const Command &command) {
  os << "Command{action=" << command.action << ", "
     << "file='" << command.file << "', "
     << "unsavedFile='" << command.unsavedFile << "', "
     << "dir='" << command.dir << "', "
     << "line=" << command.line << ", "
     << "column=" << command.column << ", "
     << "prefix='" << command.prefix << "', "
     << "caseStyle='" << command.style << "', "
     << "flags=[";
  bool first = true;
  for (const std::string &flag : command.flags) {
    if (!first)
      os << ", ";
    os << "'" << flag << "'";
    first = false;
  }
  os << "], "
     << "opt=" << (command.opt ? "on" : "off");

  return os << "}";
}

static Command::Action actionFromString(const std::string &actionStr) {
#define X(sym, str, help)                                                      \
  if (actionStr == str)                                                        \
    return Command::sym;

#include "Commands.def"

  return Command::Unknown;
}

CommandParser::CommandParser() : tempFile_("irony-server") {
}

Command *CommandParser::parse(const std::vector<std::string> &argv) {
  command_.clear();

  if (argv.begin() == argv.end()) {
    std::clog << "error: no command specified.\n"
                 "See 'irony-server help' to list available commands\n";
    return 0;
  }

  const std::string &actionStr = argv[0];

  command_.action = actionFromString(actionStr);

  bool readCompileOptions = false;
  std::vector<std::function<bool(const std::string &)>> positionalArgs;

  switch (command_.action) {
  case Command::SetDebug:
    positionalArgs.push_back(OptionConverter(&command_.opt));
    break;

  case Command::Parse:
    positionalArgs.push_back(StringConverter(&command_.file));
    readCompileOptions = true;
    break;

  case Command::Complete:
    positionalArgs.push_back(StringConverter(&command_.file));
    positionalArgs.push_back(UnsignedIntConverter(&command_.line));
    positionalArgs.push_back(UnsignedIntConverter(&command_.column));
    readCompileOptions = true;
    break;

  case Command::GetType:
    positionalArgs.push_back(UnsignedIntConverter(&command_.line));
    positionalArgs.push_back(UnsignedIntConverter(&command_.column));
    break;

  case Command::SetUnsaved:
    positionalArgs.push_back(StringConverter(&command_.file));
    positionalArgs.push_back(StringConverter(&command_.unsavedFile));
    break;

  case Command::ResetUnsaved:
    positionalArgs.push_back(StringConverter(&command_.file));
    break;

  case Command::Candidates:
    positionalArgs.push_back(StringConverter(&command_.prefix));
    positionalArgs.push_back(PrefixMatchStyleConverter(&command_.style));
    break;
  case Command::CompletionDiagnostics:
  case Command::Diagnostics:
  case Command::Help:
  case Command::Exit:
    // no-arguments commands
    break;

  case Command::GetCompileOptions:
    positionalArgs.push_back(StringConverter(&command_.dir));
    positionalArgs.push_back(StringConverter(&command_.file));
    break;

  case Command::Unknown:
    std::clog << "error: invalid command specified: " << actionStr << "\n";
    return 0;
  }

  auto argsBegin = argv.begin() + 1;
  const auto argsEnd = std::find(argsBegin, argv.end(), "--");
  const int argCount = std::distance(argsBegin, argsEnd);

  // compile options are provided after '--'
  if (readCompileOptions && argsEnd != argv.end()) {
    command_.flags.assign(std::next(argsEnd), argv.end());
  }

  if (argCount != static_cast<int>(positionalArgs.size())) {
    std::clog << "error: invalid number of arguments for '" << actionStr
              << "' (requires " << positionalArgs.size() << " got " << argCount
              << ")\n";
    return 0;
  }

  for (auto fn : positionalArgs) {
    if (!fn(*argsBegin)) {
      std::clog << "error: parsing command '" << actionStr
                << "': invalid argument '" << *argsBegin << "'\n";
      return 0;
    }
    ++argsBegin;
  }

  // '-' is used as a special file to inform that the buffer hasn't been saved
  // on disk and only the buffer content is available. libclang needs a file, so
  // this is treated as a special value for irony-server to create a temporary
  // file for this. note that libclang will gladly accept '-' as a filename but
  // we don't want to let this happen since irony already reads stdin.
  if (command_.file == "-") {
    command_.file = tempFile_.getPath();
  }

  return &command_;
}
