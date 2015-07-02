/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief Translation Unit manager.
 *
 * Keeps a cache of translation units, reparsing or recreating them as
 * necessary.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#ifndef IRONY_MODE_SERVER_TUMANAGER_H_
#define IRONY_MODE_SERVER_TUMANAGER_H_

#include "support/CIndex.h"
#include "support/NonCopyable.h"

#include <map>
#include <string>
#include <vector>

class TUManager : public util::NonCopyable {
public:
  TUManager();
  ~TUManager();

  /**
   * \brief Parse \p filename with flag \p flags.
   *
   * The first time call \c clang_parseTranslationUnit() and save the TU in the
   * member \c translationUnits_, The next call with the same \p filename will
   * call \c clang_reparseTranslationUnit().
   *
   * usage:
   * \code
   * std::vector<std::string> flags;
   * flags.push_back("-I../utils");
   * CXTranslationUnit tu = tuManager.parse("file.cpp", flags);
   *
   * if (! tu)
   *   std::cerr << "parsing translation unit failed\n";
   * \endcode
   *
   * \return The translation unit, if the parsing failed the translation unit
   *         will be \c NULL.
   */
  CXTranslationUnit parse(const std::string &filename,
                          const std::vector<std::string> &flags,
                          const std::vector<CXUnsavedFile> &unsavedFiles);

  /**
   * \brief Retrieve, creating it if necessary the TU associated to filename.
   *
   * \return The translation unit. Will be NULL if the translation unit couldn't
   *         be created.
   */
  CXTranslationUnit getOrCreateTU(const std::string &filename,
                          const std::vector<std::string> &flags,
                          const std::vector<CXUnsavedFile> &unsavedFiles);

  /**
   * \brief Invalidate a given cached TU, the next use of a TU will require
   *        reparsing.
   *
   * This can be useful for example: when the flags used to compile a file have
   * changed.
   *
   * \param filename    The filename for which the associated
   *                    translation unit flags need to be invalidated.
   *
   * \sa invalidateAllCachedTUs()
   */
  void invalidateCachedTU(const std::string &filename);

  /**
   * \brief Invalidate all cached TU, the next use of a TU will require
   *        reparsing.
   *
   * \sa invalidateCachedTU()
   */
  void invalidateAllCachedTUs();

private:
  /**
   * \brief Get a reference to the translation unit that matches \p filename
   *        with the given set of flags.
   *
   * The TU will be null if it has never been parsed or if the flags have
   * changed.
   *
   * \todo Find a proper name.
   */
  CXTranslationUnit &tuRef(const std::string &filename,
                           const std::vector<std::string> &flags);

private:
  typedef std::map<const std::string, CXTranslationUnit> TranslationUnitsMap;
  typedef std::map<const std::string, std::vector<std::string>> FilenameFlagsMap;

private:
  CXIndex index_;
  TranslationUnitsMap translationUnits_; // cache variable
  FilenameFlagsMap flagsPerFileCache_;
  unsigned parseTUOptions_;
};

#endif /* !IRONY_MODE_SERVER_TUMANAGER_H_ */
