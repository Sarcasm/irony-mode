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
  /// \sa diagnostics(), getType()
  void parse(const std::string &file,
             const std::vector<std::string> &flags,
             const std::vector<CXUnsavedFile> &unsavedFiles);

  /// \}

  /// \name Observers
  /// \{

  /// \brief Retrieve the last parse diagnostics for the given file.
  void diagnostics() const;

  /// \brief Get types of symbol at a given location.
  ///
  /// Example:
  ///
  /// \code
  ///   typedef int MyType;
  ///   MyType a;
  /// \endcode
  ///
  /// Type of cursor location for 'a' is:
  ///
  /// \code{.el}
  ///    ("MyType" "int")
  /// \endcode
  ///
  /// TODO: test with CXString(), seems to be twice the same string
  ///
  void getType(unsigned line, unsigned col) const;

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

  /// \brief Prints t if the compilation database is activated, nil otherwise.
  static void hasCompilationDatabase();

  /// \brief Get compile options from JSON database.
  ///
  /// \param databaseFile File containing the compilation commands.
  /// \param file File to obtain compile commands for.
  ///
  /// Example output:
  /// \code{.el}
  ///    (
  ///     (("-Wfoo" "-DBAR" "-Iqux") . "/path/to/working/directory")
  ///     (("-Wfoo-alt" "-DBAR_ALT" "-Iqux/alt") . "/alt/working/directory")
  ///    )
  /// \endcode
  ///
  void getCompileOptions(const std::string &databaseFile,
                         const std::string &file);

  /// \}

  /// \brief Guess compile options of file not present in the database, e.g. a
  /// header.
  ///
  /// \param databaseFile File containing the compilation commands.
  /// \param file File to guess compile commands for.
  ///
  /// The guessed command and the name of the source file are printed in Elisp
  /// format to stdout.
  ///
  /// Example output:
  /// \code{.el}
  ///    ("/path/to/guessed/file.cpp" .
  ///       (("-Wfoo" "-DBAR" "-Iqux") . "/path/to/working/directory")))
  ///    )
  /// \endcode
  ///
  void guessCompileOptions(const std::string &databaseFile,
                           const std::string &file);

private:
  TUManager tuManager_;
  CXTranslationUnit activeTu_;
  std::string file_;
  bool debug_;
  CompilationDatabase database_;
};

#endif // IRONY_MODE_SERVER_IRONY_H_
