#include "CompDBCache.h"

#include <sys/stat.h>

#include <cassert>

CompDBCache::CompDBCache()
  : db_(nullptr), mtime_(0) {
}

CompDBCache::~CompDBCache() {
  clear();
}

CXCompilationDatabase CompDBCache::fromDirectory(const std::string &buildDir,
                                                 CXCompilationDatabase_Error *error) {
  assert(error != nullptr);

  const std::string jsonFilename = constructJsonDbFilename(buildDir);
  const time_t mtime = modificationTime(jsonFilename);

  if (jsonFilename == filename_ && mtime != 0 && mtime == mtime_) {
    // Using the cached compilation database.
    // Just set the provided error code to indicate success.
    *error = CXCompilationDatabase_NoError;
  } else {
    clear();

    db_ = clang_CompilationDatabase_fromDirectory(buildDir.c_str(), error);

    if (mtime != 0 && *error == CXCompilationDatabase_NoError) {
      // Successfully loaded a JSON compilation database.
      // Cache the result.
      filename_ = jsonFilename;
      mtime_ = mtime;
    }
  }

  return db_;
}

void CompDBCache::clear() {
  if (db_) {
    clang_CompilationDatabase_dispose(db_);
    db_ = nullptr;
    filename_.clear();
    mtime_ = 0;
  }
}

std::string CompDBCache::constructJsonDbFilename(const std::string &buildDir) const {
  std::string ret = buildDir;
  if (!buildDir.empty() && buildDir.back() != '/')
    ret += '/';
  ret += "compile_commands.json";
  return ret;
}

time_t CompDBCache::modificationTime(const std::string &filename) const {
  time_t mtime = 0;
#ifdef _WIN32
  struct _stat st;
  const int statRes = _stat(filename.c_str(), &st);
#else
  struct stat st;
  const int statRes = stat(filename.c_str(), &st);
#endif
  if (statRes == 0)
    mtime = st.st_mtime;
  return mtime;
}
