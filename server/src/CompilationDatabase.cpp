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
  FileHandle(std::string fileName, const char *mode)
    : fp_(fopen(fileName.c_str(), mode)) {
  }
  ~FileHandle() {
    if (fp_)
      fclose(fp_);
  }

  FILE *fp_;
};

std::vector<std::string> CompileCommand::splitCommand(const std::string &Command) {
  std::vector<std::string> cmdSplit;
  std::istringstream iss(Command);

  std::string cmdArg;
  while (iss >> cmdArg)
    cmdSplit.emplace_back(cmdArg);

  return cmdSplit;
}

time_t CompilationDatabase::getModTime(const std::string &fileName) {
  // TODO: Add suport for Windows
  time_t time = 0;

  struct stat dbStats;
  if (stat(fileName.c_str(), &dbStats) != 0)
    return time;

  time = dbStats.st_atime;

  return time;
}

void CompilationDatabase::readOrUpdateDatabase(const std::string &fileName) {
  if (databaseFile_ != fileName ||
      difftime(getModTime(fileName), readTime_) >= 0.0) {
    std::clog << "I: Reloading database.\n";
    readDatabase(fileName);
  }
}

void CompilationDatabase::readDatabase(const std::string &fileName) {
  FileHandle file(fileName, "r");

  if (!file.fp_) {
    std::clog << "I: Couldn't open compilation database file!\n";
    return;
  }

  char buffer[bufferSize];
  rapidjson::FileReadStream stream{file.fp_, buffer, sizeof(buffer)};
  // TODO: What if the database isn't encoded as UTF8?
  rapidjson::EncodedInputStream<rapidjson::UTF8<>, rapidjson::FileReadStream>
    encStream{stream};

  rapidjson::Document doc;
  doc.ParseStream(encStream);

  if (doc.HasParseError()) {
    std::clog << "I: Error parsing compilation database file!\n";
    return;
  }

  if (!doc.IsArray()) {
    std::clog << "I: Expected top level array when reading compilation "
                 "database!\n";
    return;
  }

  // If we've gotten this far without failure, consider the database read. Set
  // the name of the database file, record the time of reading and clear the
  // compile commands.
  databaseFile_ = fileName;
  readTime_ = time(nullptr);
  cmdMap_.clear();

  for (unsigned i = 0, e = doc.Size(); i != e; ++i) {
    rapidjson::Value &compileCmd = doc[i];

    if (!compileCmd.IsObject() || !compileCmd.HasMember("file") ||
        !compileCmd.HasMember("directory") ||
        !compileCmd.HasMember("command")) {
      std::clog << "I: Badly formatted compile command in database!\n";
      continue;
    }

    std::string file{compileCmd["file"].GetString()};
    CompileCommand cmd{std::string{compileCmd["directory"].GetString()},
                       std::string{compileCmd["command"].GetString()}};
    cmdMap_.emplace(std::move(file), std::move(cmd));
  }
}

void CompilationDatabase::printDatabase() const {
  for (const auto &cmd_pair : cmdMap_) {
    std::cout << "file: " << cmd_pair.first << "\n"
              << "directory: " << cmd_pair.second.dir_ << "\n";

    std::cout << "command:";
    for (const std::string &cmdArg : cmd_pair.second.cmd_)
      std::cout << " " << cmdArg;
    std::cout << "\n";
  }
}

std::vector<const CompileCommand *>
CompilationDatabase::getCommands(const std::string &srcFile) const {
  std::vector<const CompileCommand*> cmds;

  auto range = cmdMap_.equal_range(srcFile);
  cmds.reserve(std::distance(range.first, range.second));

  std::transform(range.first, range.second, std::back_inserter(cmds),
                 [] (const FileMapType::value_type &v) { return &v.second; });

  return cmds;
}
