#ifndef IRONY_MODE_SERVER_QUOTEDCXSTRING_H_
#define IRONY_MODE_SERVER_QUOTEDCXSTRING_H_

#include "clang-c/Index.h"
#include <string>

class QuotedCXString
{
private:
  CXString cxstring_;

public:
  QuotedCXString(const CXString & cxstring);
  ~QuotedCXString();
  std::string asString() const;
};

#endif /* !IRONY_MODE_SERVER_QUOTEDCXSTRING_H_ */
