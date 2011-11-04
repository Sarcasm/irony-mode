/**
 * \file   CodeCompletion.hh
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Thu Jul 21 00:08:54 2011
 *
 * \brief  Completion plugin class definition.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef _IRONY_SERVER_CODECOMPLETION_HH_
#define _IRONY_SERVER_CODECOMPLETION_HH_

#include <string>

#include "clang-c/Index.h"

#include "IPlugin.hh"

/**
 * A completion plugin, using the libclang completion features.
 *
 */
class CodeCompletion : public IPlugin
{
public:
  CodeCompletion();
  virtual ~CodeCompletion();

  /**
   * Execute a completion request.
   *
   * \see \c IPlugin.
   *
   * \param TUManager
   * \param data
   * \param buf         A string describing the possible completions.
   *
   */
  virtual std::string handleRequest(TUManager &               tuManager,
                                    const JSONObjectWrapper & data,
                                    std::string &             buf);

private:
  /**
   * Execute a code completion at line \p line and column \p column in
   * the file \p filename. The result of the completion should be
   * added to the parameter \p buf.
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
   * Format the completion string \p completionString into the buffer
   * \p buff in an Emacs Lisp structure.
   *
   * \param completionString
   * \param buf
   */
  void formatCompletionString(CXCompletionString & completionString,
                              std::string &        buf);

  /**
   * Format if possible the given kind to a lisp keyword symbol.
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
   * Add a result cons cell.
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
   * \param needQuote   \c true if the value need to be quoted, \c
   *                    false otherwise (default to false).
   */
  void appendConsCellResult(const std::string & keyword,
                            const std::string & value,
                            std::string &       buf,
                            bool                needQuote = false);

private:
  CodeCompletion(CodeCompletion const &);
  CodeCompletion& operator=(CodeCompletion const &);
};


#endif /* !_IRONY_SERVER_CODECOMPLETION_HH_ */
