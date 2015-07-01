/**
 * \file
 *
 * \brief Using libclang for reading the JSON compilation database.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */
#include <iostream>

#include <clang-c/Index.h>
#include <clang-c/CXCompilationDatabase.h>
#include <clang-c/CXString.h>

#include "CompilationDatabase.h"

static void getExactCompCmds(CXCompilationDatabase &db,
                             const std::string &fullFilename,
                             std::vector<std::vector<std::string>> &flags) {
  CXCompileCommands cmds =
      clang_CompilationDatabase_getCompileCommands(db, fullFilename.c_str());
  unsigned ncmd = clang_CompileCommands_getSize(cmds);
  flags.resize(ncmd);

  for (unsigned i = 0; i < ncmd; ++i) {
    CXCompileCommand cmd = clang_CompileCommands_getCommand(cmds, 0);
    CXString cxdir = clang_CompileCommand_getDirectory(cmd);

    const char *dir = clang_getCString(cxdir);
    flags[i].push_back(dir);
    clang_disposeString(cxdir);

    for (unsigned j = 0, narg = clang_CompileCommand_getNumArgs(cmd);
         j < narg; ++j) {
      CXString str = clang_CompileCommand_getArg(cmd, j);
      const char *cstr = clang_getCString(str);
      flags[i].push_back(cstr);
      clang_disposeString(str);
    }
  }

  clang_CompileCommands_dispose(cmds);
}

std::vector<std::vector<std::string>>
getCompileCommands(const std::string &buildDir,
                   const std::string &fullFilename) {
  std::vector<std::vector<std::string>> flags;
  CXCompilationDatabase db;
  CXCompilationDatabase_Error error;

  db = clang_CompilationDatabase_fromDirectory(buildDir.c_str(), &error);

  if (error == CXCompilationDatabase_CanNotLoadDatabase) {
    std::clog << "Cannot load database!\n"
              << "Build directory: " << buildDir << "\n"
              << "File name: " << fullFilename << "\n";
    return flags;
  }

  getExactCompCmds(db, fullFilename, flags);

  clang_CompilationDatabase_dispose(db);

  return flags;
}
