/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief irony-server "API" declarations.
 *
 * Contains the commands that the Emacs package relies on. These commands are
 * mostly wrappers around a subset of the features provided by libclang. Command
 * results are printed to \c std::cout as s-expr, in order to make it easy for
 * Emacs to consume.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#ifndef IRONY_MODE_SERVER_IRONY_H_
#define IRONY_MODE_SERVER_IRONY_H_

#include "TUManager.h"

#include <iosfwd>

class Irony {
public:
  Irony();

  bool isDebugEnabled() const {
    return debug_;
  }

  /// \name Commands
  /// @{

  /// \brief Set or unset debugging of commands.
  void setDebug(bool enable) {
    debug_ = enable;
  }

  /// \brief Retrieve the diagnostics for the given file.
  void diagnostics(const std::string &file,
                   const std::vector<std::string> &flags,
                   const std::vector<CXUnsavedFile> &unsavedFiles);

  /// \brief Perform code completion at a given location.
  ///
  /// Print the list of candidate if any. The empty list is printed on error.
  ///
  /// Example output:
  ///
  /// \code{.el}
  ///    (
  ///     ("foo")
  ///     ("bar")
  ///     ("baz")
  ///    )
  /// \endcode
  ///
  void complete(const std::string &file,
                unsigned line,
                unsigned col,
                const std::vector<std::string> &flags,
                const std::vector<CXUnsavedFile> &unsavedFiles);
  /// @}

private:
  TUManager tuManager_;
  bool debug_;
};

#endif // IRONY_MODE_SERVER_IRONY_H_
