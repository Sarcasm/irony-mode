/**
 * \file   ClangString.h
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Wed Feb  6 17:48:40 2013
 *
 * \brief  Wrapper class around CXStrings.
 *
 */

#ifndef IRONY_MODE_SERVER_CLANGSTRING_H_
#define IRONY_MODE_SERVER_CLANGSTRING_H_

// clang-c/CXString.h is not used directly here because it wasn't
// available in old version that irony-server still supports, no big
// deal.
#include <clang-c/Index.h>

#include <string>

/**
 * \note The string will be disposed in the destructor.
 */
class ClangString
{
public:
  // \warning The implementation is quite dumb, if some flags are
  //          added it will probably be necessary to rewrite it.
  enum Flags {
    // blah"huh -> blah\"huh
    Escape = 0x01,

    // imply Escape
    // toto     -> "toto"
    // note: Quote will be added as necessary: toto\"ohoh -> "toto\"ohoh"
    AddQuotes = (0x02 | Escape)
  };
private:
  CXString    cxstring_;
  const char *cstr_;
  unsigned    flags_;

public:
  ClangString(const CXString & cxstring,
              unsigned         flags = 0U);
  ~ClangString();

  bool isNull() const;

  /**
   * If \c isNull() return \c true then consider the string to be the
   * empty string.
   */
  std::string asString() const;
};

#endif /* !IRONY_MODE_SERVER_CLANGSTRING_H_ */
