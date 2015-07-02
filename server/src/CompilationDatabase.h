/**-*-C++-*-
 * \file
 *
 * \brief Routines for reading compilation databases.
 *
 * Will contain functions for reading JSON and clang_complete
 * compilation databases when ready. Early stage of development.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#ifndef IRONY_COMPILATION_DATABASE_H
#define IRONY_COMPILATION_DATABASE_H

#include <vector>
#include <string>

// Return all compile commands and the associated working directory
// for a file.  For every command, the working directory is the first
// entry in the list of compiler flags.
std::vector<std::vector<std::string>> getFlags(const std::string &projectRoot,
                                               const std::string &fullFilename);

#endif
