/** -*- mode: C++ -*-
 *
 *  \file
 *  \author Kirill Ignatiev <github.com/ikirill>
 *  \brief Printing libclang AST objects in sexp form.
 *
 *  This file is distributed under the GNU General Public License. See
 *  COPYING for details.
 */

#ifndef IRONYMODE_SERVER_SRC_SUPPORT_SEXP_H_
#define IRONYMODE_SERVER_SRC_SUPPORT_SEXP_H_

#include <clang-c/Index.h>
#include <iostream>
#include <string>
#include <sstream>

namespace sexp {

namespace detail {

template <typename T>
struct sexp_proxy;
struct comment_proxy;
template <typename T>
struct alistEntry_proxy;
struct completion_proxy;
}

/** Return printable representation of value as an emacs-lisp object.

    There is a special implementation for type int that prints -1 as nil.

    When printing a representation of a CXString, the string will be
    destroyed after printing (repr takes ownership).
*/
template <typename T>
detail::sexp_proxy<T> repr(const T &value) {
  return {value};
}

/** String representation of a cursor's brief comment. */
detail::comment_proxy comment(CXCursor);

/** Completion strings. */
detail::completion_proxy completion(CXCursor);
detail::completion_proxy completion(CXCompletionString);

/** Print an alist entry of the form " (name . value)", omitting it if
    value is nil, and printing parens nicely if value is a list.
    Value is expected to be a single sexp and not a cons pair.
*/
template <typename T>
detail::alistEntry_proxy<T> alistEntry(const std::string &name,
                                       const T &value) {
  return {name, value};
}

// Implementation

namespace detail {

template <typename T>
struct sexp_proxy {
  sexp_proxy(T value) : value(value) {
  }
  T value;
};

template <>
struct sexp_proxy<CXString> {
  sexp_proxy(CXString value) : value(value) {
  }
  ~sexp_proxy() {
    clang_disposeString(value);
  }
  CXString value;
};

std::ostream &operator<<(std::ostream &, const sexp_proxy<CXString> &);
std::ostream &operator<<(std::ostream &, const sexp_proxy<CXCursorKind> &);
std::ostream &operator<<(std::ostream &, const sexp_proxy<CXTypeKind> &);
std::ostream &operator<<(std::ostream &, const sexp_proxy<bool> &);
std::ostream &operator<<(std::ostream &, const sexp_proxy<int> &);

/// Comment information for a given cursor.
struct comment_proxy {
  comment_proxy(const CXCursor &cursor) : cursor(cursor) {
  }
  const CXCursor &cursor;
};
std::ostream &operator<<(std::ostream &, const comment_proxy &);

/// See alistEntry.
template <typename T>
struct alistEntry_proxy {
  alistEntry_proxy(std::string name, const T &value)
    : name_(name), value_(value) {
  }
  std::string name_;
  const T &value_;
};

template <typename T>
std::ostream &operator<<(std::ostream &out, const alistEntry_proxy<T> &proxy) {
  std::ostringstream sstream;
  sstream << proxy.value_;
  std::string s = sstream.str();
  if (s == "nil")
    return out;
  if (s[0] == '(' && s[s.size() - 1] == ')') {
    int first_nonspace = 0;
    while (s[++first_nonspace] == ' ')
      ;
    out << " (" << proxy.name_ << " " << s.substr(first_nonspace);
  } else {
    out << " (" << proxy.name_ << " . " << s << ")";
  }
  return out;
}

/// Completion string
struct completion_proxy {
  completion_proxy(CXCompletionString completion) : completion(completion) {
  }
  CXCompletionString completion;
};
std::ostream &operator<<(std::ostream &, const completion_proxy &);

} // namespace detail
} // namespace sexp

#endif // IRONYMODE_SERVER_SRC_SUPPORT_SEXP_H_
