/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief ClangString implementation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "ClangString.h"

#include <algorithm>
#include <cstring>
#include <iostream>

ClangString::ClangString(const CXString &cxstring, unsigned flags)
  : cxstring_(cxstring), cstr_(clang_getCString(cxstring_)), flags_(flags) {
  if (flags_ & AddQuotes)
    flags_ |= ClangString::Escape;
}

ClangString::~ClangString() {
  clang_disposeString(cxstring_);
}

namespace {
bool needEscape(char ch) {
  return ch == '\\' || ch == '"';
}
}

bool ClangString::isNull() const {
  return cstr_ == 0;
}

std::string ClangString::asString() const {
  const char *cStr = isNull() ? "" : cstr_;
  std::size_t len = std::strlen(cStr);
  std::size_t finalSize = len;

  if (!(flags_ & Escape)) {
    return std::string(cStr, finalSize);
  }

  if (flags_ & AddQuotes) {
    finalSize += 2;
  }

  std::string quotedStr;
  std::size_t escapes = std::count_if(cStr, cStr + len, needEscape);

  finalSize += escapes;
  quotedStr.reserve(finalSize);

  if (flags_ & AddQuotes) {
    quotedStr += '"';
  }

  if (!escapes) {
    quotedStr.append(cStr, len);
  } else {
    for (std::size_t i = 0; i < len; ++i) {
      if (needEscape(cStr[i])) {
        quotedStr += '\\';
      }
      quotedStr += cStr[i];
    }
  }

  if (flags_ & AddQuotes) {
    quotedStr += '"';
  }

  return quotedStr;
}
