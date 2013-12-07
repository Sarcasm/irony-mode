/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief Completion plugin class declaration.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_PLUGINS_CODECOMPLETION_H_
#define IRONY_MODE_SERVER_PLUGINS_CODECOMPLETION_H_

#include "IPlugin.h"

#include "util/NonCopyable.h"

#include <clang-c/Index.h>

#include <string>
#include <vector>

class Candidate;
typedef std::vector<Candidate> CompletionResults;

/**
 * \brief A completion plugin, using the libclang completion features.
 *
 */
class CodeCompletion : public IPlugin, public util::NonCopyable {
public:
  /**
   * \brief Create a completion plugin.
   *
   * \param TUManager
   * \param detailedCompletions
   *    If \c false only \c CXCompletionChunk_TypedText will be present in the
   *    completion result (unlike a list of cons cells).
   *
   * \return
   */
  CodeCompletion(TUManager &tuManager, bool detailedCompletions);
  virtual ~CodeCompletion();

  /**
   * \brief Execute a completion request.
   *
   * \see \c IPlugin.
   *
   * \param data
   * \param [out] out
   *    A stream to print the possible completions.
   *
   */
  virtual std::string handleRequest(const JSONObjectWrapper &data,
                                    std::ostream &out);

private:
  /**
   * Execute a code completion at line \p line and column \p column in the file
   * \p filename.
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
  bool complete(CXTranslationUnit &tu,
                const std::string &filename,
                unsigned line,
                unsigned column,
                std::ostream &out);

  /**
   * Print results with detailed information, such as result type, priority,
   * optionals, ...
   *
   * A result look like this (start with a list, and the cdr is an
   * assoc list):
   *
\verbatim
;; (result-list (p . PRIORITY-NUMBER) [(opt . t)])
(("ptrdiff_t") (p . 50))
(("basic_ios" ?< (ph . "typename _CharT") (opt ?, (ph . "typename_Traits")) ?>)
(p . 50) (opt . t))
(((r . "bool") "uncaught_exception" ?( ?)) (p . 50))
(("std" (t . "::")) (p . 75))
\endverbatim
   *
   * \note: The typed text will always be the only element of type
   *        string in the result list (stringp -> t).
   *        - (r . "bool") 'r for result-type
   *        - (ph . "value") for place holders.
   *        - (t . "::") 't for text to be inserted
   *        - (i . " const") 'i for information, such as constness of
   *          a function but that doesn't need to be inserted
   *        - `?c` for characters, such as comma, brakets, ...
   *        - `(opt *result-list)` to start an optional chunk. Note
   *          that optional chunks do not require typed-text but
   *          allows inner opt.
   *        - (p . "int a") for the current parameter of a function.
   *
   * If (opt . t) is given, it means that the result contains optional
   * fragments that may be expanded.
   */
  void printDetailedResults(const CompletionResults &completions,
                            std::ostream &out);

  /**
   * Log the diagnostics found during completion.
   *
   * \param completions
   */
  void handleDiagnostics(CXCodeCompleteResults *completions) const;

private:
  TUManager &tuManager_;
  TUManager::SettingsID settingsID_;
  bool detailedCompletions_;
};

#endif /* !IRONY_MODE_SERVER_PLUGINS_CODECOMPLETION_H_ */
