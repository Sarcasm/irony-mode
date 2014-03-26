/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief CompileChecker plugin declaration.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_PLUGINS_COMPILECHECKER_H_
#define IRONY_MODE_SERVER_PLUGINS_COMPILECHECKER_H_

#include "IPlugin.h"

#include "util/NonCopyable.h"

#include <clang-c/Index.h>

/**
 * \brief Simple plugin answering a "compile-check" command.
 *
 * This command send some statistics back about the compilation of a
 * translation unit, such as the number of errors.
 */
class CompileChecker : public IPlugin, public util::NonCopyable {
public:
  CompileChecker(TUManager &tuManager);
  virtual ~CompileChecker();

  /**
   * \brief Execute a "compile-check" request.
   *
   * Fill the following information into \p buf:
   *
   * The plist entry \c :stats with value:
   * - \c t \n
   *   Means the compilation is okay.
   *
   * - \c nil \n
   *   Means a translation unit haven't been made at all, that's bad.
   *
   * - a \b plist \n
   *   All keys are optional keys. Each of them take a <b>strictly
   *   positive</b> integer as argument: the number of diagnostic of
   *   this kind. Here is the list of valid keys:
   *
   *   - \c :fatal-errors
   *   - \c :errors
   *   - \c :warnings
   *
   * Example output:
   *
\verbatim
  :stats (:errors 3 :warnings 15)
  :stats t     ;; no error, file is fine
  :stats nil   ;; 'unknown' error
\endverbatim
   *
   * \sa IPlugin
   *
   * \param data
   * \param buf
   */
  virtual std::string handleRequest(const JSONObjectWrapper &data,
                                    std::ostream &out);

private:
  TUManager &tuManager_;
};

#endif /* !IRONY_MODE_SERVER_PLUGINS_COMPILECHECKER_H_ */
