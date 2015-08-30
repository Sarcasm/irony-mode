/** -*- C++ -*-
 * \file
 * \author Karl Hylén <karl.hylen@gmail.com>
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#include "CompilationDatabase.h"

#include <boost/filesystem.hpp>
#include <boost/property_tree/json_parser.hpp>

#include <algorithm>
#include <cstdio>
#include <fstream>
#include <iostream>

struct FileHandle {
  FileHandle(const std::string &fileName, const char *mode)
    : fp_(fopen(fileName.c_str(), mode)) {
  }
  ~FileHandle() {
    if (fp_)
      fclose(fp_);
  }

  FILE *fp_;
};

std::vector<std::string>
CompileCommand::splitCommand(const std::string &Command) {
  std::vector<std::string> cmdSplit;
  std::istringstream iss(Command);

  std::string cmdArg;
  while (iss >> cmdArg)
    cmdSplit.emplace_back(cmdArg);

  return cmdSplit;
}

time_t CompilationDatabase::getModTime(const std::string &fileName) {
  boost::system::error_code error;
  time_t time = boost::filesystem::last_write_time(fileName, error);

  // Print warning and return 0
  if (error) {
    std::clog << "Warning: Couldn't get modification time for : " << fileName
              << ".\n"
              << "Got error code " << error << "\n";
    return 0u;
  }

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
  boost::property_tree::ptree pt;

  std::clog << "I: Reading database using boost!\n";

  try {
    boost::property_tree::json_parser::read_json(fileName, pt);
  } catch (boost::property_tree::ptree_error E) {
    std::clog << "ERROR: Couldn't read the JSON compile commands file: "
              << E.what() << "\n";
    return;
  }

  // If we've gotten this far without failure, consider the database read. Set
  // the name of the database file, record the time of reading and clear the
  // compile commands.
  databaseFile_ = fileName;
  readTime_ = time(nullptr);
  cmdMap_.clear();

  try {
    for (auto I = pt.begin(), E = pt.end(); I != E; ++I) {
      std::string file{I->second.get<std::string>("file")};
      CompileCommand cmd{I->second.get<std::string>("directory"),
                         I->second.get<std::string>("command")};
      cmdMap_.emplace(std::move(file), std::move(cmd));
    }
  } catch (boost::property_tree::ptree_error E) {
    std::clog << "I: Badly formatted compile command in database!\n";

    // Reset the database file
    databaseFile_ = "";
    cmdMap_.clear();

    return;
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

static unsigned calcDirScore(boost::filesystem::path &left,
                             boost::filesystem::path right) {
  using path = boost::filesystem::path;

  path leftDir{left.parent_path()};
  path rightDir{right.parent_path()};

  path::iterator leftItr = leftDir.end(), leftBeg = leftDir.begin();
  path::iterator rightItr = rightDir.end(), rightBeg = rightDir.begin();

  unsigned score = 0;
  while (leftItr != leftBeg && rightItr != rightBeg) {
    --leftItr;
    --rightItr;

    if (*leftItr == *rightItr)
      ++score;
    else
      break;
  }

  return score;
}

static unsigned levenshteinDistance(const std::string &left,
                                    const std::string &right) {
  std::vector<std::vector<unsigned>> matrix;

  // Construct matrix
  matrix.reserve(left.size() + 1);
  for (unsigned i = 0; i < left.size() + 1; ++i)
    matrix.emplace_back(right.size() + 1);

  // Initialize first column and row
  for (unsigned i = 0; i < left.size() + 1; ++i)
    matrix[i][0] = i;
  for (unsigned j = 0; j < right.size() + 1; ++j)
    matrix[0][j] = j;

  // Fill the matrix
  for (unsigned i = 1; i < left.size() + 1; ++i) {
    for (unsigned j = 1; j < right.size() + 1; ++j) {
      matrix[i][j] = std::min(
          {matrix[i - 1][j    ] + 1,
           matrix[i    ][j - 1] + 1,
           matrix[i - 1][j - 1] + (left[i - 1] == right[j - 1] ? 0 : 1)});
    }
  }

  return matrix[left.size()][right.size()];
}

std::pair<const std::string *, const CompileCommand *>
CompilationDatabase::guessCommand(const std::string &srcFile) const {
  using path = boost::filesystem::path;
  path srcPath{srcFile};

  if (cmdMap_.empty())
    return std::make_pair(nullptr, nullptr);

  // Step 1: Match exact without file extension
  for (const auto &pair : cmdMap_) {
    path dbFilePath{pair.first};

    if (srcPath.stem() == dbFilePath.stem())
      return std::make_pair(&pair.first, &pair.second);
  }

  // Step 2: Filter out entries that have as many direct parents in the path as
  // possible in common with srcFile
  using CmdPair = std::pair<const std::string *, const CompileCommand *>;
  std::vector<CmdPair> filteredCmds;
  filteredCmds.reserve(size());

  unsigned maxScore = 0;
  for (const auto &pair : cmdMap_) {
    unsigned score = calcDirScore(srcPath, path{pair.first});

    if (score > maxScore) {
      filteredCmds.clear();
      maxScore = score;
    }

    if (score == maxScore)
      filteredCmds.emplace_back(&pair.first, &pair.second);
  }

  // Step 3: Get best matching filename using levenshtein distance
  unsigned minScore = std::numeric_limits<unsigned>::max();
  CmdPair minElem{nullptr, nullptr};
  for (const CmdPair scoreEntry : filteredCmds) {
    path entryPath{*scoreEntry.first};
    unsigned score = levenshteinDistance(entryPath.stem().native(),
                                           srcPath.stem().native());

    if (score < minScore) {
      minScore = score;
      minElem = scoreEntry;
    }
  }

  return minElem;
}
