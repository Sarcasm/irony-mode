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

#ifndef IRONY_DATABASE_H
#define IRONY_DATABASE_H

#include <vector>
#include <string>

std::pair<std::string, std::vector<std::string>> getFlags(
    const std::string &directory,
    const std::string &filename);

#endif
