/**-*-C++-*-
 * \file
 * \author Idar Tollefsen <idart@hotmail.com>
 *
 * \brief Creates a compilation database and caches it.
 *
 * Keeps a cache of the loaded compilation database, only creating a new
 * one when needed.
 *
 * This file is distributed under the GNU General Public License.
 * See COPYING for details.
 */

#ifndef IRONY_MODE_COMPDBCACHE_H_
#define IRONY_MODE_COMPDBCACHE_H_

#include "support/CIndex.h"
#include "support/NonCopyable.h"

#include <ctime>
#include <string>

class CompDBCache : public util::NonCopyable {
public:
  CompDBCache();
  ~CompDBCache();

  /**
   * \brief Get a compilation database from a database found in a directory.
   *
   * This in essence a wrapper around
   * \c clang_CompilationDatabase_fromDirectory() with added caching.
   * It will either create a new compilation database or return the
   * already loaded one (if any).
   *
   * This class owns the resulting compilation database.
   * Therefore, unlike \c clang_CompilationDatabase_fromDirectory(),
   * callers must NOT call \c clang_CompilationDatabase_dispose() on the
   * returned compilation database. This class will handle that internally.
   *
   * \param buildDir Directory containing the database (such as
   *                 "compile_commands.json") to create a compilation
   *                 database from.
   * \param error Error code from attempting to create a compilation
   *              database (\c CXCompilationDatabase_NoError on success).
   *
   * \return The compilation database or nullptr.
   */
  CXCompilationDatabase fromDirectory(const std::string &buildDir,
                                      CXCompilationDatabase_Error *error);

private:
  /**
   * \brief Clear the cache.
   *
   * This will dispose the currently loaded compilation database (if any) by
   * calling \c clang_CompilationDatabase_dispose() on it. And it will reset
   * other internal housekeeping variables related to the caching of the
   * compilation database.
   */
  void clear();

  /**
   * \brief Construct JSON compilation database filename.
   *
   * \param buildDir Directory that might contain "compile_commands.json".
   *
   * \return Path to "compilation_commands.json" in \c buildDir.
   */
  std::string constructJsonDbFilename(const std::string &buildDir) const;

  /**
   * \brief Get modification time of a file.
   *
   * \param filename The file to get last modification time of.
   *
   * \return The modification time of \c filename or 0.
   */
  time_t modificationTime(const std::string &filename) const;

  CXCompilationDatabase db_;
  std::string filename_;
  time_t mtime_;
};

#endif
