/**-*-C++-*-
 * \file   SyntaxChecker.h
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Wed Aug 24 13:52:10 2011
 *
 * \brief  Syntax checker plugin declaration.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_PLUGINS_SYNTAXCHECKER_H_
#define IRONY_MODE_SERVER_PLUGINS_SYNTAXCHECKER_H_

#include "IPlugin.h"

#include <clang-c/Index.h>

#include "util/NonCopyable.h"

/**
 * \brief A syntax checking plugin, give informations about the
 *        translation unit parsing. Errors found, possible fix, etc.
 *
 */
class SyntaxChecker : public IPlugin,
                      public util::NonCopyable
{
private:
  TUManager & tuManager_;

public:
  SyntaxChecker(TUManager & tuManager);
  virtual ~SyntaxChecker();

  /**
   * \brief Execute a "check syntax" request.
   *
   * \see \c IPlugin.
   *
   * \param data
   * \param buf         A string describing parsing errors.
   */
  virtual std::string handleRequest(const JSONObjectWrapper & data,
                                    std::string &             buf);

private:
  /**
   * \brief Format the Clang diagnostic \p diagnostic into an Emacs
   *        Lisp object written in \p buf.
   *
   * \param diagnostic  The diagnostic to format.
   * \param [out] buf   The buffer where the diagnostic should be
   *                    written.
   */
  void formatDiagnostic(const CXDiagnostic & diagnostic,
                        std::string &        buf);

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
   * \param [out] buf   The string buffer where the location should be
   *                    added.
   *
   */
  void formatSourceLocation(const CXSourceLocation & location,
                            std::string &            buf);

  /**
   * \brief Format a range, a range is a cons of 2 sources locations.
   *
   * \code
   * (("/bar.cpp" 27 (3 . 1)) . ("/bar.cpp" 29 (3 . 2)))
   * \endcode
   *
   * \param range       The range to format.
   * \param [out] buf   Where the range should be written.
   */
  void formatSourceRange(const CXSourceRange & range,
                         std::string &         buf);

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
   * \param [out] buf   Where the range should be written.
   */
  void formatFitItHints(const CXDiagnostic & diagnostic,
                        std::string &        buf);
};

#endif /* !IRONY_MODE_SERVER_PLUGINS_SYNTAXCHECKER_H_ */
