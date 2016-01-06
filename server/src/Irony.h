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
#include "CompilationDatabase.h"

#include <string>
#include <vector>

class Irony {
public:
  Irony();

  bool isDebugEnabled() const {
    return debug_;
  }

  /// \name Modifiers
  /// \{

  /// \brief Set or unset debugging of commands.
  void setDebug(bool enable) {
    debug_ = enable;
  }

  /// Parse or reparse the given file and compile options.
  ///
  /// If the compile options have changed, the translation unit is re-created to
  /// take this into account.
  ///
  /// Output \c nil or \c t, whether or not parsing the translation unit
  /// succeeded.
  ///
  /// \sa diagnostics()
  void parse(const std::string &file,
             const std::vector<std::string> &flags,
             const std::vector<CXUnsavedFile> &unsavedFiles);

  /// \}

  /// \name Observers
  /// \{

  /// \brief Retrieve the last parse diagnostics for the given file.
  void diagnostics() const;

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

  /// \brief Get compile options from JSON database.
  ///
  /// \param DatabaseFile File containing the compilation commands.
  /// \param File File to obtain compile commands for.
  ///
  /// Example output:
  ///
  /// \code{.el}
  ///    (
  ///     (("-Wfoo" "-DBAR" "-Iqux") . "/path/to/working/directory")
  ///     (("-Wfoo-alt" "-DBAR_ALT" "-Iqux/alt") . "/alt/working/directory")
  ///    )
  /// \endcode
  ///
  void getCompileOptions(const std::string &DatabaseFile,
                         const std::string &File);

  /// \}

  void guessCompileOptions(const std::string &DatabaseFile,
                           const std::string &File);

private:
  TUManager tuManager_;
  CXTranslationUnit activeTu_;
  bool debug_;
  CompilationDatabase database;
};

#endif // IRONY_MODE_SERVER_IRONY_H_
