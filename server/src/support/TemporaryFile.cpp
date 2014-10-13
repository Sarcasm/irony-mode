/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#include "TemporaryFile.h"

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <random>

static std::string getTemporaryFileDirectory() {
  const char *temporaryDirEnvVars[] = {"TMPDIR", "TMP", "TEMP", "TEMPDIR"};

  for (const char *envVar : temporaryDirEnvVars) {
    if (const char *dir = std::getenv(envVar))
      return dir;
  }

  return "/tmp";
}

TemporaryFile::TemporaryFile(const std::string &prefix,
                             const std::string &suffix)
  : pathOrPattern_(prefix + "-%%%%%%" + suffix) {
}

TemporaryFile::~TemporaryFile() {
  if (openedFile_) {
    openedFile_.reset();
    std::remove(pathOrPattern_.c_str());
  }
}

const std::string &TemporaryFile::getPath() {
  if (!openedFile_) {
    openedFile_.reset(new std::fstream);

    std::random_device rd;
    std::default_random_engine e(rd());
    std::uniform_int_distribution<int> dist(0, 15);
    std::string pattern = pathOrPattern_;
    std::string tmpDir = getTemporaryFileDirectory() + "/";
    int i = 0;

    do {
      // exiting is better than infinite loop
      if (++i > TemporaryFile::MAX_ATTEMPS) {
        std::cerr << "error: couldn't create temporary file, please check your "
                     "temporary file directory (" << tmpDir << ")\n";
        exit(EXIT_FAILURE);
      }

      // make the filename based on the pattern
      std::transform(pattern.begin(),
                     pattern.end(),
                     pathOrPattern_.begin(),
                     [&e, &dist](char ch) {
        return ch == '%' ? "0123456789abcdef"[dist(e)] : ch;
      });
      // create the file
      openedFile_->open(tmpDir + pathOrPattern_, std::ios_base::out);
    } while (!openedFile_->is_open());
    pathOrPattern_ = tmpDir + pathOrPattern_;
  }

  return pathOrPattern_;
}
