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
  CommandLineArgumentParser(const std::string &commandLine)
    : input_(commandLine), position_(input_.begin() - 1) {
  }

  std::vector<std::string> parse() {
    bool hasMoreInput = true;
    while (hasMoreInput && nextNonWhitespace()) {
      std::string argument;
      hasMoreInput = parseStringInto(argument);
      commandLine_.push_back(argument);
    }
    return commandLine_;
  }

private:
  // All private methods return true if there is more input available.

  bool parseStringInto(std::string &string) {
    do {
      if (*position_ == '"') {
        if (!parseDoubleQuotedStringInto(string))
          return false;
      } else if (*position_ == '\'') {
        if (!parseSingleQuotedStringInto(string))
          return false;
      } else {
        if (!parseFreeStringInto(string))
          return false;
      }
    } while (*position_ != ' ');
    return true;
  }

  bool parseDoubleQuotedStringInto(std::string &string) {
    if (!next())
      return false;
    while (*position_ != '"') {
      if (!skipEscapeCharacter())
        return false;
      string.push_back(*position_);
      if (!next())
        return false;
    }
    return next();
  }

  bool parseSingleQuotedStringInto(std::string &string) {
    if (!next())
      return false;
    while (*position_ != '\'') {
      string.push_back(*position_);
      if (!next())
        return false;
    }
    return next();
  }

  bool parseFreeStringInto(std::string &string) {
    do {
      if (!skipEscapeCharacter())
        return false;
      string.push_back(*position_);
      if (!next())
        return false;
    } while (*position_ != ' ' && *position_ != '"' && *position_ != '\'');
    return true;
  }

  bool skipEscapeCharacter() {
    if (*position_ == '\\') {
      return next();
    }
    return true;
  }

  bool nextNonWhitespace() {
    do {
      if (!next())
        return false;
    } while (*position_ == ' ');
    return true;
  }

  bool next() {
    ++position_;
    return position_ != input_.end();
  }

private:
  const std::string input_;
  std::string::const_iterator position_;
  std::vector<std::string> commandLine_;
};

} // unnamed namespace

std::vector<std::string>
unescapeCommandLine(const std::string &escapedCommandLine) {
  CommandLineArgumentParser parser(escapedCommandLine);
  return parser.parse();
}
