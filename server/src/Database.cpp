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

#include "Database.h"

static void getExactFlags(std::vector<std::vector<std::string>> &flags,
                          CXCompilationDatabase &db,
                          const std::string &fullFilename) {
  unsigned ncmd;
  unsigned i;
  CXCompileCommands cmds;

  cmds = clang_CompilationDatabase_getCompileCommands(db, fullFilename.c_str());
  ncmd = clang_CompileCommands_getSize(cmds);
  flags.resize(ncmd);

  for (i = 0; i < ncmd; ++i) {
    unsigned narg;
    unsigned j;
    CXCompileCommand cmd;
    CXString cxdir;
    const char *dir;

    cmd = clang_CompileCommands_getCommand(cmds, 0);
    cxdir = clang_CompileCommand_getDirectory(cmd);
    dir = clang_getCString(cxdir);
    flags[i].push_back(dir);

    narg = clang_CompileCommand_getNumArgs(cmd);
    for (j = 0; j < narg; ++j) {
      CXString str;
      const char *cstr;

      str = clang_CompileCommand_getArg(cmd, j);
      cstr = clang_getCString(str);
      flags[i].push_back(cstr);
    }
  }

  clang_CompileCommands_dispose(cmds);
}

std::vector<std::vector<std::string>>
getFlags(const std::string &projectRoot, const std::string &fullFilename) {
  std::vector<std::vector<std::string>> flags;
  CXCompilationDatabase db;
  CXCompilationDatabase_Error error;

  db = clang_CompilationDatabase_fromDirectory(projectRoot.c_str(), &error);

  if (error == CXCompilationDatabase_CanNotLoadDatabase) {
    std::clog << "Cannot load database!\n";
    return flags;
  }

  getExactFlags(flags, db, fullFilename);

  clang_CompilationDatabase_dispose(db);

  return flags;
}
