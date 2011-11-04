/**
 * \file   TUManager.hh
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Tue Aug 23 23:39:33 2011
 *
 * \brief  Translation Unit manager, this is used for "caching" a
 *         translation unit will be reparsed but not re-created if
 *         possible, etc.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#ifndef _IRONY_SERVER_TUMANAGER_HH_
#define _IRONY_SERVER_TUMANAGER_HH_

#include "JSONValue.h"
#include "JSON.h"

#include "clang-c/Index.h"

#include <map>
#include <string>
#include <vector>

class TUManager
{
private:
  CXIndex                                        index_;
  std::map<const std::string, CXTranslationUnit> translationUnits_; // cache

public:
  TUManager();
  virtual ~TUManager();

  /**
   * The first time call \c clang_parseTranslationUnit() and save the
   * TU in the member \c translationUnits_, the next call with the
   * same \p filename will call \c clang_reparseTranslationUnit().
   *
   * usage:
   * \code
   * std::vector<std::string> flags;
   * flags.push_back("-I../utils");
   * CXTranslationUnit tu = tuManager.parse("file.cpp", flags);
   *
   * if (! tu)
   *   std::cerr << "parsing translation unit failed" << std::endl;
   * \endcode
   *
   * \param filename    The filename to parse.
   * \param flags       The compiler flags (typically -I dir).
   *
   * \return The translation unit, if the parsing failed the
   *         translation unit will be \c NULL.
   */
  CXTranslationUnit parse(const std::string &              filename,
                          const std::vector<std::string> & flags);

  /**
   * Convert the JSONArray of string representing the compiler flags
   * into a vector of strings. This is an utility function aimed to
   * simplify the parsing of flags for the different plugins.
   *
   * \param flags The list of compiler flags (i.e. "-I../include",
   *              "-I.." ...).
   *
   * \return A vector of compiler flags (can be empty).
   */
  static std::vector<std::string> getFlags(const JSONValue * flags);

private:
  TUManager(TUManager const &);
  TUManager& operator=(TUManager const &);
};

#endif /* !_IRONY_SERVER_TUMANAGER_HH_ */
