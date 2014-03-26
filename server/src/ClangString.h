/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief Wrapper class around CXStrings.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_SERVER_CLANGSTRING_H_
#define IRONY_MODE_SERVER_CLANGSTRING_H_

// clang-c/CXString.h is not used directly here because it wasn't available in
// old version that irony-server still supports.
#include <clang-c/Index.h>

#include <string>

/**
 * \brief RAII class that wraps a CXString with some convenience methods.
 */
class ClangString {
public:
  // \warning The implementation is quite dumb, if some flags are added it will
  //          probably be necessary to rewrite it.
  enum Flags {
    // blah"huh -> blah\"huh
    Escape = 0x01,

    // toto -> "toto"
    // (also implies Escape)
    AddQuotes = 0x02
  };

public:
  ClangString(const CXString &cxstring, unsigned flags = 0U);
  ~ClangString();

  bool isNull() const;

  /**
   * If \c isNull() return \c true then consider the string to be the empty
   * string.
   */
  std::string asString() const;

private:
  ClangString(const ClangString &);
  ClangString &operator=(const ClangString &);

private:
  CXString cxstring_;
  const char *cstr_;
  unsigned flags_;
};

#endif /* !IRONY_MODE_SERVER_CLANGSTRING_H_ */
