/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief Not the best piece of code out there.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#ifndef IRONY_MODE_SERVER_SUPPORT_TEMPORARY_FILE_H_
#define IRONY_MODE_SERVER_SUPPORT_TEMPORARY_FILE_H_

#include <iosfwd>
#include <memory>
#include <string>

class TemporaryFile {
  enum {
    // if we can't create the temp file, exits.
    MAX_ATTEMPS = 25
  };

public:
  TemporaryFile(const std::string &prefix, const std::string &suffix = "");
  ~TemporaryFile();

  /// Returns the path of this temporary filename
  const std::string &getPath();

private:
  std::string pathOrPattern_;
  std::unique_ptr<std::fstream> openedFile_;
};

#endif // IRONY_MODE_SERVER_SUPPORT_TEMPORARY_FILE_H_
