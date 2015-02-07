// -*- mode: c++ -*-
#ifndef IRONYMODE_SERVER_SRC_SUPPORT_SEXP_H_
#define IRONYMODE_SERVER_SRC_SUPPORT_SEXP_H_

#include <clang-c/Index.h>
#include <iostream>
#include <string>
#include <sstream>

namespace sexp {

namespace detail {

template <typename T> struct sexp_proxy;
struct cursorRef_proxy;
struct type_proxy;
struct comment_proxy;
template <typename T> struct alistEntry_proxy;

}

/** Return printable representation of value as an emacs-lisp object.

    There is a special implementation for type int that prints -1 as nil.

    When printing cursors, the information about their parents is
    unreliable (I think), so an AST walker should do this on its own.

    When printing a representation of a CXString, the string will be
    destroyed after printing (repr takes ownership).
*/
template <typename T>
detail::sexp_proxy<T> repr(const T& value) { return {value}; }

/** Sexp representation of a pointer to another cursor
    (actually, of the form (cursor-ref HASH)). */
detail::cursorRef_proxy cursorRef(CXCursor);

/** Representation of type information for a cursor. */
detail::type_proxy type(CXCursor);

/** String representation of a cursor's brief comment. */
detail::comment_proxy comment(CXCursor cursor);

/** Print an alist entry of the form " (name . value)", omitting it if
    value is nil, and printing parens nicely if value is a list.
    Value is expected to be a single sexp.
*/
template <typename T>
detail::alistEntry_proxy<T> alistEntry(const std::string& name, const T& value) { return {name, value}; }


/* Implementation. */

namespace detail {

template <typename T>
struct sexp_proxy {
  sexp_proxy(const T& value) : value(value) { }
  const T& value;
};

template <>
struct sexp_proxy<CXString> {
  sexp_proxy(const CXString& value) : value(value) { }
  ~sexp_proxy() { clang_disposeString(value); }
  const CXString& value;
};

std::ostream& operator<<(std::ostream&, const sexp_proxy<CXCursor>&);

std::ostream& operator<<(std::ostream&, const sexp_proxy<CXString>&);
std::ostream& operator<<(std::ostream&, const sexp_proxy<CXCursorKind>&);
std::ostream& operator<<(std::ostream&, const sexp_proxy<CXTypeKind>&);
std::ostream& operator<<(std::ostream&, const sexp_proxy<bool>&);
std::ostream& operator<<(std::ostream&, const sexp_proxy<int>&);
std::ostream& operator<<(std::ostream&, const sexp_proxy<CXType>&);

struct cursorRef_proxy {
  cursorRef_proxy(CXCursor other_cursor)
    : other_cursor(other_cursor) { }
  CXCursor other_cursor;
};
std::ostream& operator<<(std::ostream&, const cursorRef_proxy&);

inline
cursorRef_proxy cursorRef(CXCursor cursor) { return cursorRef_proxy(cursor); }

/// Type information for CXCursors
struct type_proxy {
  type_proxy(const CXCursor& cursor) : cursor(cursor) { }
  const CXCursor& cursor;
};
std::ostream& operator<<(std::ostream&, const type_proxy&);

inline
type_proxy type(CXCursor cursor) { return type_proxy(cursor); }

/// Comment information for a given cursor.
struct comment_proxy {
  comment_proxy(const CXCursor& cursor) : cursor(cursor) { }
  const CXCursor& cursor;
};
std::ostream& operator<<(std::ostream&, const comment_proxy&);

/// See alistEntry.
template <typename T>
struct alistEntry_proxy {
  alistEntry_proxy(std::string name, const T& value)
    : name_(name), value_(value) { }
  std::string name_;
  const T& value_;
};

template <typename T>
std::ostream& operator<<(std::ostream& out, const alistEntry_proxy<T>& proxy) {
  std::ostringstream sstream;
  sstream << proxy.value_;
  std::string s = sstream.str();
  if (s == "nil") return out;
  if (s[0] == '(' && s[s.size()-1] == ')') {
    out << " (" << proxy.name_ << " " << s.substr(1);
  } else {
    out << " (" << proxy.name_ << " . " << s << ")";
  }
  return out;
}

} // namespace detail
} // namespace sexp

#endif // IRONYMODE_SERVER_SRC_SUPPORT_SEXP_H_
