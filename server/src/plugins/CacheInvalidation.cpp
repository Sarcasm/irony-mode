/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief See CacheInvalidation.h
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "CacheInvalidation.h"

#include <iostream>

CacheInvalidation::CacheInvalidation(TUManager &tuManager)
  : tuManager_(tuManager) {
}

CacheInvalidation::~CacheInvalidation() {
}

std::string CacheInvalidation::handleRequest(const JSONObjectWrapper &data,
                                             std::ostream &out) {
  bool valid = true;
  const std::string &file = data.check(L"file", valid);

  out << ":value ";
  if (!valid) {
    out << ":error";
    std::clog << "invalid reload-flags request." << std::endl;
  } else {
    tuManager_.invalidateCachedTU(file);
    out << ":success";
  }
  return ":status-code";
}
