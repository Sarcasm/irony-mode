/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief Syntax checker plugin declaration.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_PLUGINS_SYNTAXCHECKER_H_
#define IRONY_MODE_SERVER_PLUGINS_SYNTAXCHECKER_H_

#include "IPlugin.h"

#include "util/NonCopyable.h"

#include <clang-c/Index.h>

/**
 * \brief A syntax checking plugin, give informations about the
 *        translation unit parsing. Errors found, possible fix, etc.
 *
 */
class SyntaxChecker : public IPlugin, public util::NonCopyable {
public:
  SyntaxChecker(TUManager &tuManager);
  virtual ~SyntaxChecker();

  /**
   * \brief Execute a "check syntax" request.
   *
   * \see \c IPlugin.
   *
   * \param data
   * \param [out] out    A stream to print parsing errors.
   */
  virtual std::string handleRequest(const JSONObjectWrapper &data,
                                    std::ostream &out);

private:
  /**
   * \brief Format the Clang diagnostic \p diagnostic into an Emacs
   *        Lisp object written in \p out.
   *
   * \param diagnostic  The diagnostic to format.
   * \param [out] out   The stream where the diagnostic should be written.
   */
  void formatDiagnostic(const CXDiagnostic &diagnostic, std::ostream &out);

  /**
   * \brief Format the source location \p location into an Emacs Lisp
   *        expression of the following form:
   *
   * \code
   * ;; ("filename" offset (line . column))
   * ;; i.e:
   *  ("/home/user/project/foo/bar.cpp" 27 (3 . 1))
   * ;; i.e:
   * nil ;the null location
   * \endcode
   *
   * Note: NULL source location are allowed, in this case the location
   * string will be \c nil.
   *
   * \param location    The location to format.
   * \param [out] out   The stream where the location should be added.
   *
   */
  void formatSourceLocation(const CXSourceLocation &location,
                            std::ostream &out);

  /**
   * \brief Format a range, a range is a cons of 2 sources locations.
   *
   * \code
   * (("/bar.cpp" 27 (3 . 1)) . ("/bar.cpp" 29 (3 . 2)))
   * \endcode
   *
   * \param range       The range to format.
   * \param [out] out   Where the range should be written.
   */
  void formatSourceRange(const CXSourceRange &range, std::ostream &out);

  /**
   * \brief Format diagnostic fix-it hints. Fix-its hints is a list of
   *        cons.
   *
   * The cons is of the form:
   *
   * \code
   * ("fix-it" . range) ;see \c format
   * \endcode
   *
   * \param fixIt       The range to format.
   * \param [out] out   Where the range should be written.
   */
  void formatFitItHints(const CXDiagnostic &diagnostic, std::ostream &out);

private:
  TUManager &tuManager_;
};

#endif /* !IRONY_MODE_SERVER_PLUGINS_SYNTAXCHECKER_H_ */
