/**
 * \file   CodeCompletion.h
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Thu Jul 21 00:08:54 2011
 *
 * \brief  Completion plugin class declaration.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_PLUGINS_CODECOMPLETION_H_
#define IRONY_MODE_SERVER_PLUGINS_CODECOMPLETION_H_

#include "IPlugin.h"

#include <clang-c/Index.h>
#include <string>

#include "util/NonCopyable.h"

/**
 * \brief A completion plugin, using the libclang completion features.
 *
 */
class CodeCompletion : public IPlugin,
                       public util::NonCopyable
{
private:
  TUManager &           tuManager_;
  TUManager::SettingsID settingsID_;
  bool                  detailedCompletions_;

public:
  /**
   * \brief Create a completion plugin.
   *
   * \param TUManager
   * \param detailedCompletions If \c false only
   *                            \c CXCompletionChunk_TypedText will be
   *                            present in the completion result
   *                            (unlike a list of cons cells).
   *
   * \return
   */
  CodeCompletion(TUManager & tuManager, bool detailedCompletions);
  ~CodeCompletion();

  /**
   * \brief Execute a completion request.
   *
   * \see \c IPlugin.
   *
   * \param data
   * \param buf
   *            A string describing the possible completions.
   *
   */
  virtual std::string handleRequest(const JSONObjectWrapper & data,
                                    std::string &             buf);

private:
  /**
   * \brief Execute a code completion at line \p line and column \p
   *        column in the file \p filename.
   *
   * The result of the completion is added to the parameter \p buf.
   *
   * \param tu          The translation unit of the file \p filename.
   * \param filename    The name of the file where the code completion
   *                    should be executed.
   * \param line        The line where the code completion should occur.
   * \param column      The column where the code completion should
   *                    occur.
   * \param buf         The string result of the code completion, a
   *                    s-expression.
   *
   * \return \c true if the code completion succeed, otherwise
   *         \c false and the value of \c buf is undefined.
   */
  bool complete(CXTranslationUnit & tu,
                const std::string & filename,
                unsigned            line,
                unsigned            column,
                std::string &       buf);

  /**
   * \brief Format a cursor kind to a keyword symbol.
   *
   * \param cursorKind
   * \param buf         The string where the keyword symbol should be
   *                    added.
   */
  void formatCompletionCursorKind(CXCursorKind cursorKind, std::string & buf);

  /**
   * \brief Format the completion string \p completionString into the
   *        buffer \p buff in an Emacs Lisp structure.
   *
   * \param completionString
   * \param buf
   */
  void formatCompletionString(CXCompletionString & completionString,
                              std::string &        buf);

  /**
   * \brief Format if possible the given kind to a lisp keyword
   *        symbol.
   *
   * \param kind The kind to format.
   * \param buf The buffer to append the text.
   *
   * \return \c true if the formatting to a keyword symbol was
   *         possible, otherwise \c false.
   */
  bool tryFormattingKeywordSymbol(CXCompletionChunkKind kind,
                                  std::string &         buf);

  /**
   * \brief Add a result cons cell.
   *
   * The following cons is inserted in \p buf.
   * \verbatim
   *  (:keyword . "value") ;needQuote = true
   *  (:keyword . value)   ;needQuote = false
   * \endverbatim
   *
   * \param keyword
   * \param value
   * \param buf         The string where the text shoulb be added.
   */
  void appendConsCellResult(const std::string & keyword,
                            const std::string & value,
                            std::string &       buf);
};

#endif /* !IRONY_MODE_SERVER_PLUGINS_CODECOMPLETION_H_ */
