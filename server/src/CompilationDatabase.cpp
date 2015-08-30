/** -*- C++ -*-
 * \file
 * \author Karl Hylén <karl.hylen@gmail.com>
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#include "CompilationDatabase.h"

#include "rapidjson/document.h"
#include "rapidjson/filereadstream.h"
#include "rapidjson/encodedstream.h"

// TODO: ifdef linux/mac
#include <sys/stat.h>
// endif

#include <algorithm>
#include <cstdio>
#include <fstream>
#include <iostream>

struct FileHandle {
  FILE *fp;
  FileHandle(std::string FileName, const char *Mode)
    : fp(fopen(FileName.c_str(), Mode)) {
  }
  ~FileHandle() {
    if (fp)
      fclose(fp);
  }
};

std::vector<std::string> CompileCommand::splitCommand(const std::string &Command) {
  std::vector<std::string> CmdSplit;
  std::istringstream S(Command);

  std::string CmdArg;
  while (S >> CmdArg)
    CmdSplit.emplace_back(CmdArg);

  return CmdSplit;
}

time_t CompilationDatabase::getModTime(const std::string &FileName) {
  // TODO: Add suport for Windows
  time_t time = 0;

  struct stat dbStats;
  if (stat(FileName.c_str(), &dbStats) != 0)
    return time;

  time = dbStats.st_atime;

  return time;
}

void CompilationDatabase::readOrUpdateDatabase(const std::string &FileName) {
  if (DatabaseFile != FileName ||
      difftime(getModTime(FileName), ReadTime) >= 0.0) {
    std::clog << "I: Reloading database.\n";
    readDatabase(FileName);
  }
}

void CompilationDatabase::readDatabase(const std::string &filename) {
  FileHandle file(filename, "r");

  if (!file.fp) {
    std::clog << "I: Couldn't open compilation database file!\n";
    return;
  }

  char buffer[bufferSize];
  rapidjson::FileReadStream stream{file.fp, buffer, sizeof(buffer)};
  // TODO: What if the database isn't encoded as UTF8?
  rapidjson::EncodedInputStream<rapidjson::UTF8<>, rapidjson::FileReadStream>
    encStream{stream};

  rapidjson::Document Doc;
  Doc.ParseStream(encStream);

  if (!Doc.IsArray()) {
    std::clog << "I: Expected top level array when reading compilation "
                 "database!\n";
    return;
  }

  // If we've gotten this far without failure, consider the database read. Set
  // the name of the database file, record the time of reading and clear the
  // compile commands.
  DatabaseFile = filename;
  ReadTime = time(nullptr);
  CmdMap.clear();

  for (unsigned i = 0, e = Doc.Size(); i != e; ++i) {
    rapidjson::Value &CompileCmd = Doc[i];

    if (!CompileCmd.IsObject() || !CompileCmd.HasMember("file") ||
        !CompileCmd.HasMember("directory") ||
        !CompileCmd.HasMember("command")) {
      std::clog << "I: Badly formatted compile command in database!\n";
      continue;
    }

    std::string File{CompileCmd["file"].GetString()};
    CompileCommand cmd{std::string{CompileCmd["directory"].GetString()},
                       std::string{CompileCmd["command"].GetString()}};
    CmdMap.emplace(std::move(File), std::move(cmd));
  }
}

void CompilationDatabase::printDatabase() const {
  for (const auto &cmd_pair : CmdMap) {
    std::cout << "file: " << cmd_pair.first << "\n"
              << "directory: " << cmd_pair.second.Dir << "\n";

    std::cout << "command:";
    for (const std::string &CmdArg : cmd_pair.second.Cmd)
      std::cout << " " << CmdArg;
    std::cout << "\n";
  }
}

std::vector<const CompileCommand *>
CompilationDatabase::getCommands(const std::string &File) const {
  std::vector<const CompileCommand*> Cmds;

  auto Range = CmdMap.equal_range(File);
  Cmds.reserve(std::distance(Range.first, Range.second));

  std::transform(Range.first, Range.second, std::back_inserter(Cmds),
                 [] (const FileMapType::value_type &v) { return &v.second; });

  return Cmds;
}
