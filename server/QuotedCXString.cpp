#include "QuotedCXString.h"

#include <algorithm>
#include <cstring>

QuotedCXString::QuotedCXString(const CXString & cxstring)
  : cxstring_(cxstring)
{ }

QuotedCXString::~QuotedCXString()
{
  clang_disposeString(cxstring_);
}

namespace {

bool needEscape(char ch)
{
  return ch == '\\' || ch == '"';
}

}

std::string QuotedCXString::asString() const
{
  const char  *cStr    = clang_getCString(cxstring_);
  std::size_t  len     = std::strlen(cStr);
  std::size_t  escapes = std::count_if(cStr, cStr + len, needEscape);
  std::string  quotedStr;

  quotedStr.reserve(len + escapes);
  quotedStr += '"';
  if (escapes) {
    for (std::size_t i = 0; i < len; ++i) {
      if (needEscape(cStr[i])) {
        quotedStr += '\\';
      }
      quotedStr += cStr[i];
    }
  } else {
    std::string(cStr, len);
  }
  quotedStr[0] = '"';
  return quotedStr;
}
