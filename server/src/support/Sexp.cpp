/**
 *  \file
 *  \author Kirill Ignatiev <github.com/ikirill>
 *  \brief Printing libclang AST objects in sexp form.
 *
 *  This file is distributed under the GNU General Public License. See
 *  COPYING for details.
 */

#include "Sexp.h"
#include "iomanip_quoted.h"

#include "Sexp_CursorKind.cpp"
#include "Sexp_TypeKind.cpp"

#include <sstream>
#include <utility>
#include <array>
#include <algorithm>

namespace sexp {

/** String representation of a cursor's brief comment. */
detail::comment_proxy comment(CXCursor cursor) {
  return {cursor};
}

detail::completion_proxy completion(CXCursor cursor) {
  return {clang_getCursorCompletionString(cursor)};
}

detail::completion_proxy completion(CXCompletionString completion) {
  return {completion};
}

namespace detail {

/** Libclang seems to return empty strings when there is nothing
    meaningful to return, so empty strings are printed as nil. */
std::ostream &operator<<(std::ostream &out,
                         const sexp_proxy<CXString> &string) {
  const char *data = clang_getCString(string.value);
  if (!data)
    return out << "nil";
  std::string s = std::string(data);
  if (s == "")
    return out << "nil";
  return out << support::quoted(s);
}

std::ostream &operator<<(std::ostream &out, const sexp_proxy<int> &integer) {
  if (integer.value == -1)
    return out << "nil";
  return out << integer.value;
}

std::ostream &operator<<(std::ostream &out, const sexp_proxy<bool> &boolean) {
  return out << (boolean.value ? "t" : "nil");
}

std::ostream &operator<<(std::ostream &out, const comment_proxy &proxy) {
  CXString comment = clang_Cursor_getBriefCommentText(proxy.cursor);
  if (!clang_getCString(comment)) {
    CXCursor cursor = clang_getCursorDefinition(proxy.cursor);
    comment = clang_Cursor_getBriefCommentText(cursor);
  }
  if (!clang_getCString(comment)) {
    CXCursor cursor = clang_getCursorReferenced(proxy.cursor);
    comment = clang_Cursor_getBriefCommentText(cursor);
  }
  if (clang_getCString(comment))
    out << repr(comment);
  else
    out << "nil";
  return out;
}

std::string chunkKind(CXCompletionChunkKind kind) {
  static const std::array<const std::string,
                          CXCompletionChunk_VerticalSpace + 2> names{
      {"Optional",
       "TypedText",
       "Text",
       "Placeholder",
       "Informative",
       "CurrentParameter",
       "LeftParen",
       "RightParen",
       "LeftBracket",
       "RightBracket",
       "LeftBrace",
       "RightBrace",
       "LeftAngle",
       "RightAngle",
       "Comma",
       "ResultType",
       "Colon",
       "SemiColon",
       "Equal",
       "HorizontalSpace",
       "VerticalSpace",
       "UnknownCXCompletionChunkKind"}};
  return names[std::min((size_t)kind, names.size()-1)];
}

std::ostream &operator<<(std::ostream &out, const completion_proxy &proxy) {
  CXCompletionString completion = proxy.completion;
  if (!completion)
    return out << "nil";
  out << "(completion"
      << alistEntry("parent",
                    repr(clang_getCompletionParent(completion, nullptr)))
      << alistEntry("comment",
                    repr(clang_getCompletionBriefComment(completion)))
      << alistEntry("priority", clang_getCompletionPriority(completion));

  unsigned annot = clang_getCompletionNumAnnotations(completion);
  if (annot) {
    out << " (annotations";
    for (unsigned i = 0; i < annot; ++i) {
      out << " " << repr(clang_getCompletionAnnotation(completion, i));
    }
    out << ")";
  }

  static const std::array<char, CXCompletionChunk_VerticalSpace + 1>
      chunk_chars = {{0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      '(',
                      ')',
                      '[',
                      ']',
                      '{',
                      '}',
                      '<',
                      '>',
                      ',',
                      0,
                      ':',
                      ';',
                      '=',
                      ' ',
                      '\n'}};

  // We will make the completion chunks presentable later, in lisp
  unsigned numchunks = clang_getNumCompletionChunks(completion);
  out << " (chunks";
  std::ostringstream s;
  for (unsigned i = 0; i < numchunks; ++i) {
    auto kind = clang_getCompletionChunkKind(completion, i);

    if (kind < chunk_chars.size() && chunk_chars[kind]) {
      s << chunk_chars[kind];
      continue;
    }
    if (kind == CXCompletionChunk_TypedText) {
      CXString text = clang_getCompletionChunkText(completion, i);
      s << clang_getCString(text);
      clang_disposeString(text);
      continue;
    }
    if (s.tellp()) {
      out << " " << support::quoted(s.str());
      s = std::ostringstream();
    }

    out << " (" << chunkKind(kind) << " . ";
    if (kind == CXCompletionChunk_Optional) {
      out << sexp::completion(
          clang_getCompletionChunkCompletionString(completion, i));
    } else {
      out << repr(clang_getCompletionChunkText(completion, i));
    }
    out << ")";
  }
  if (s.tellp())
    out << " " << support::quoted(s.str());
  out << ")"; // chunks

  return out << ")";
}

} // namespace detail
} // namespace sexp
