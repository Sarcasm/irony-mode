/** -*- C++ -*-
 * \file
 * \author Karl Hylén <karl.hylen@gmail.com>
 *
 * \brief Classes for reading JSON compilation database.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#ifndef COMPILATION_DATABASE_H
#define COMPILATION_DATABASE_H

#include <string>
#include <vector>
#include <sstream>
#include <unordered_map>

struct CompileCommand {
  std::string Dir;
  std::vector<std::string> Cmd;

  static std::vector<std::string> splitCommand(const std::string &Command);

  CompileCommand(const std::string &directory, const std::string &command)
    : Dir(directory), Cmd(splitCommand(command)) {
  }
  CompileCommand(std::string &&directory, std::string &&command)
    : Dir(directory), Cmd(splitCommand(command)) {
  }

  CompileCommand(const CompileCommand&) = delete;
  CompileCommand &operator=(const CompileCommand&) = delete;

  CompileCommand(CompileCommand &&) noexcept = default;
  CompileCommand &operator=(CompileCommand &&) noexcept = default;

  ~CompileCommand() = default;
};

class CompilationDatabase {
public:
  // TODO: Use something more optimal for storing file names?
  using FileMapType = std::unordered_multimap<std::string, CompileCommand>;
  using iterator = FileMapType::iterator;
  using const_iterator = FileMapType::const_iterator;

private:
  static constexpr unsigned bufferSize = 65536;

  std::string DatabaseFile{};
  time_t ReadTime{0};
  FileMapType CmdMap;

public:
  CompilationDatabase() = default;

  // Don't allow copy or move
  CompilationDatabase(const CompilationDatabase&) = delete;
  CompilationDatabase &operator=(const CompilationDatabase&) = delete;
  CompilationDatabase(CompilationDatabase&&) = delete;
  CompilationDatabase &operator=(CompilationDatabase&&) = delete;

  ~CompilationDatabase() = default;

  iterator begin() { return CmdMap.begin(); }
  const_iterator begin() const { return CmdMap.begin(); }
  iterator end() { return CmdMap.end(); }
  const_iterator end() const { return CmdMap.end(); }

  // Get all compile commands of a file
  std::vector<const CompileCommand *>
  getCommands(const std::string &FileName) const;

  void printDatabase() const;
  void readOrUpdateDatabase(const std::string &FileName);

private:
  void readDatabase(const std::string &FileName);
  static time_t getModTime(const std::string &filename);
};

#endif
