/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#include "CommandLineParser.h"

namespace {

/// \brief A parser for escaped strings of command line arguments.
///
/// Assumes \-escaping for quoted arguments (see the documentation of
/// unescapeCommandLine(...)).
class CommandLineArgumentParser {
public:
  CommandLineArgumentParser(const std::string &CommandLine)
    : Input(CommandLine), Position(Input.begin() - 1) {
  }

  std::vector<std::string> parse() {
    bool HasMoreInput = true;
    while (HasMoreInput && nextNonWhitespace()) {
      std::string Argument;
      HasMoreInput = parseStringInto(Argument);
      CommandLine.push_back(Argument);
    }
    return CommandLine;
  }

private:
  // All private methods return true if there is more input available.

  bool parseStringInto(std::string &String) {
    do {
      if (*Position == '"') {
        if (!parseDoubleQuotedStringInto(String))
          return false;
      } else if (*Position == '\'') {
        if (!parseSingleQuotedStringInto(String))
          return false;
      } else {
        if (!parseFreeStringInto(String))
          return false;
      }
    } while (*Position != ' ');
    return true;
  }

  bool parseDoubleQuotedStringInto(std::string &String) {
    if (!next())
      return false;
    while (*Position != '"') {
      if (!skipEscapeCharacter())
        return false;
      String.push_back(*Position);
      if (!next())
        return false;
    }
    return next();
  }

  bool parseSingleQuotedStringInto(std::string &String) {
    if (!next())
      return false;
    while (*Position != '\'') {
      String.push_back(*Position);
      if (!next())
        return false;
    }
    return next();
  }

  bool parseFreeStringInto(std::string &String) {
    do {
      if (!skipEscapeCharacter())
        return false;
      String.push_back(*Position);
      if (!next())
        return false;
    } while (*Position != ' ' && *Position != '"' && *Position != '\'');
    return true;
  }

  bool skipEscapeCharacter() {
    if (*Position == '\\') {
      return next();
    }
    return true;
  }

  bool nextNonWhitespace() {
    do {
      if (!next())
        return false;
    } while (*Position == ' ');
    return true;
  }

  bool next() {
    ++Position;
    return Position != Input.end();
  }

private:
  const std::string Input;
  std::string::const_iterator Position;
  std::vector<std::string> CommandLine;
};

} // unnamed namespace

std::vector<std::string>
unescapeCommandLine(const std::string &EscapedCommandLine) {
  CommandLineArgumentParser parser(EscapedCommandLine);
  return parser.parse();
}
