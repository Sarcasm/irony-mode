/**-*-C++-*-
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
  virtual ~CodeCompletion();

  /**
   * \brief Execute a completion request.
   *
   * \see \c IPlugin.
   *
   * \param data
   * \param [out] out
   *            A stream to print the possible completions.
   *
   */
  virtual std::string handleRequest(const JSONObjectWrapper & data,
                                    std::ostream &            out);

private:
  /**
   * \brief Execute a code completion at line \p line and column \p
   *        column in the file \p filename.
   *
   * The result of the completion is added to the parameter \p out.
   *
   * \param tu          The translation unit of the file \p filename.
   * \param filename    The name of the file where the code completion
   *                    should be executed.
   * \param line        The line where the code completion should occur.
   * \param column      The column where the code completion should
   *                    occur.
   * \param [out] out   The stream to print result of the code completion, a
   *                    s-expression.
   *
   * \return \c true if the code completion succeed, otherwise
   *         \c false and nothing is printed to \p out.
   */
  bool complete(CXTranslationUnit & tu,
                const std::string & filename,
                unsigned            line,
                unsigned            column,
                std::ostream &      out);

  void printDetailedResult(CXCodeCompleteResults *completions, std::ostream & out);

  /**
   * \brief Log the diagnostics found during completion.
   *
   * \param completions
   */
  void handleDiagnostics(CXCodeCompleteResults *completions) const;

  /**
   * \brief Format the completion string \p completionString into the
   *        \p out stream in an Emacs Lisp structure.
   *
   * \param completionString
   * \param [out] out
   */
  void formatCompletionString(CXCompletionString & completionString,
                              std::ostream &       out);

  /**
   * \brief Format if possible the given kind to a lisp keyword
   *        symbol.
   *
   * \param kind         The kind to format.
   * \param [out] out    The stream to append the text.
   *
   * \return \c true if the formatting to a keyword symbol was
   *         possible, otherwise \c false.
   */
  bool tryFormattingKeywordSymbol(CXCompletionChunkKind kind,
                                  std::ostream &        out);

  /**
   * \brief Add a result cons cell.
   *
   * The following cons is inserted in \p out.
   * \verbatim
   *  (:keyword . "value") ;needQuote = true
   *  (:keyword . value)   ;needQuote = false
   * \endverbatim
   *
   * \param keyword
   * \param value
   * \param [out] out      The stream where the text shoulb be added.
   */
  void appendConsCellResult(const std::string & keyword,
                            const std::string & value,
                            std::ostream &      out);

private:
  TUManager &           tuManager_;
  TUManager::SettingsID settingsID_;
  bool                  detailedCompletions_;
};

#endif /* !IRONY_MODE_SERVER_PLUGINS_CODECOMPLETION_H_ */
