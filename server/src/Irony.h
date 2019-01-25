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

#include "CompDBCache.h"
#include "Style.h"

#include <string>
#include <vector>

class Irony {
public:
  // use std::string over std::vector<char> because I have some doubts
  // that libclang expect unsaved buffers to be a null terminated C strings
  typedef std::string UnsavedBuffer;

public:
  Irony();

  bool isDebugEnabled() const {
    return debug_;
  }

  /// \name Command
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
  void parse(const std::string &file, const std::vector<std::string> &flags);

  /// Parse the given file for code completion.
  ///
  /// Shares the same semantics and output as \c parse().
  ///
  /// \sa candidates(), completionDiagnostics()
  void complete(const std::string &file,
                unsigned line,
                unsigned col,
                const std::vector<std::string> &flags);

  void setUnsaved(const std::string &file,
                  const std::string &unsavedContentFile);

  void resetUnsaved(const std::string &file);

  /// \}

  /// \name Queries
  /// \{

  /// \brief Retrieve the last parse diagnostics for the given file.
  ///
  /// \pre parse() was called.
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

  /// Get all the completion candidates.
  ///
  /// \pre complete() was called.
  void candidates(const std::string &prefix, PrefixMatchStyle style) const;

  /// Get the diagnostics produced by the last \c complete().
  ///
  /// \pre complete() was called.
  void completionDiagnostics() const;

  /// \brief Get compile options from JSON database.
  ///
  /// \param buildDir Directory containing compile_commands.json
  /// \param file File to obtain compile commands for.
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
  void getCompileOptions(const std::string &buildDir,
                         const std::string &file) const;

  /// \}

private:
  void resetCache();
  void computeCxUnsaved();

private:
  mutable CompDBCache compDBCache_;
  TUManager tuManager_;
  std::map<std::string, UnsavedBuffer> filenameToContent_;
  CXTranslationUnit activeTu_;
  std::string file_;
  std::vector<CXUnsavedFile> cxUnsavedFiles_;
  CXCodeCompleteResults *activeCompletionResults_;
  bool debug_;
};

#endif // IRONY_MODE_SERVER_IRONY_H_
