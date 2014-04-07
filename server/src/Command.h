/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_COMMAND_H_
#define IRONY_MODE_SERVER_COMMAND_H_

#include <string>
#include <vector>
#include <iosfwd>

struct Command {
  Command() {
    clear();
  }

  void clear() {
    action = Unknown;
    flags.clear();
    file.clear();
    line = 0;
    column = 0;
  }

#define X(sym, str, desc) sym,
  enum Action {
#include "Command.def"
  } action;

  std::vector<std::string> flags;
  std::string file;
  unsigned line;
  unsigned column;
};

std::ostream &operator<<(std::ostream &os, const Command::Action &action);
std::ostream &operator<<(std::ostream &os, const Command &command);

class CommandParser {
public:
  Command *parse(const std::vector<std::string> &argv);

private:
  Command command_;
};

#endif // IRONY_MODE_SERVER_COMMAND_H_
