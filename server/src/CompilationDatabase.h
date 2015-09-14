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
  static std::vector<std::string> splitCommand(const std::string &command);

  CompileCommand(const std::string &directory, const std::string &command)
    : dir_(directory), cmd_(splitCommand(command)) {
  }
  CompileCommand(std::string &&directory, std::string &&command)
    : dir_(directory), cmd_(splitCommand(command)) {
  }

  // Data members
  std::string dir_;
  std::vector<std::string> cmd_;
};

class CompilationDatabase {
public:
  // TODO: Use something more optimal for storing file names?
  using FileMapType = std::unordered_multimap<std::string, CompileCommand>;
  using size_type = FileMapType::size_type;
  using iterator = FileMapType::iterator;
  using const_iterator = FileMapType::const_iterator;

  CompilationDatabase() = default;

  // Don't allow copy or move
  CompilationDatabase(const CompilationDatabase&) = delete;
  CompilationDatabase &operator=(const CompilationDatabase&) = delete;
  CompilationDatabase(CompilationDatabase&&) = delete;
  CompilationDatabase &operator=(CompilationDatabase&&) = delete;

  ~CompilationDatabase() = default;

  iterator begin() { return cmdMap_.begin(); }
  const_iterator begin() const { return cmdMap_.begin(); }
  iterator end() { return cmdMap_.end(); }
  const_iterator end() const { return cmdMap_.end(); }

  size_type size() const { return cmdMap_.size(); }

  // Get all compile commands of a file
  std::vector<const CompileCommand *>
  getCommands(const std::string &fileName) const;

  // Guess the compilation command of a file
  std::pair<const std::string *, const CompileCommand *>
  guessCommand(const std::string &srcFile) const;

  void printDatabase() const;
  void readOrUpdateDatabase(const std::string &srcFile);

private:
  void readDatabase(const std::string &fileName);
  static time_t getModTime(const std::string &fileName);

  static constexpr unsigned bufferSize = 65536;
  std::string databaseFile_{};
  time_t readTime_{0};
  FileMapType cmdMap_;
};

#endif
