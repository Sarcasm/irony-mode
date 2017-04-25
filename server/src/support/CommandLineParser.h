/**
 * \file
 * \brief Facility to parse a command line into a string array.
 *
 * \note Please note that the code borrowed from the Clang,
 * lib/Tooling/JSONCompilationDatabase.cpp.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#ifndef IRONY_MODE_SERVER_SUPPORT_COMMAND_LINE_PARSER_H_
#define IRONY_MODE_SERVER_SUPPORT_COMMAND_LINE_PARSER_H_

#include <string>
#include <vector>

std::vector<std::string>
unescapeCommandLine(const std::string &escapedCommandLine);

#endif // IRONY_MODE_SERVER_SUPPORT_COMMAND_LINE_PARSER_H_
