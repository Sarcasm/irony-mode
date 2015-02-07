#include "Sexp.h"
#include "iomanip_quoted.h"

#include "Sexp_CursorKind.cpp"
#include "Sexp_TypeKind.cpp"

#include <sstream>
#include <cassert>
#include <utility>

namespace sexp {

/** Sexp representation of a pointer to another cursor
    (actually, of the form (cursor-ref HASH)). */
detail::cursorRef_proxy cursorRef(CXCursor cursor) {
  return detail::cursorRef(cursor);
}

/** Representation of type information for a cursor. */
detail::type_proxy type(CXCursor cursor) {
  return detail::type_proxy(cursor);
}

/** String representation of a cursor's brief comment. */
detail::comment_proxy comment(CXCursor cursor) {
  return detail::comment_proxy(cursor);
}

namespace detail {

/** Libclang seems to return empty strings when there is nothing
    meaningful to return, so empty strings are printed as nil. */
std::ostream& operator<<(std::ostream& out, const sexp_proxy<CXString>& string) {
  const char* data = clang_getCString(string.value);
  if (!data) return out << "nil";
  std::string s = std::string(data);
  if (s == "") return out << "nil";
  return out << support::quoted(s);
}

std::ostream& operator<<(std::ostream& out, const sexp_proxy<int>& integer) {
  if (integer.value == -1) return out << "nil";
  return out << integer.value;
}

std::ostream& operator<<(std::ostream& out, const sexp_proxy<bool>& boolean) {
  return out << (boolean.value ? "t" : "nil");
}

std::ostream& operator<<(std::ostream& out, const sexp_proxy<CXType>& proxy) {
  CXType type = proxy.value;
  if (type.kind == CXType_Invalid) {
    return out << "nil";
  }

  /* In principle, one could print everything libclang exposes about
     the type here, but emacs-lisp doesn't use this yet, so there is
     little reason. */

  out << "(type";
  if (type.kind == CXType_Unexposed) {
    out << " nil";
  } else {
    out << " " << repr(type.kind);
  }
  out << " (spelling . " << repr(clang_getTypeSpelling(type)) << ")";
  int num_args = clang_getNumArgTypes(type);
  if (num_args > 0) {
    out << " (args";
    for (int i = 0; i < num_args; ++i) {
      out << " " << repr(clang_getArgType(type, i));
    }
    out << ")";
    // out << alistEntry("num-args", num_args);
  }
  // out << " (decl-cursor . " << sexp(clang_getTypeDeclaration(type)) << ")";
  return out << ")";
}

std::ostream& operator<<(std::ostream& out, const type_proxy& type_proxy) {
  CXCursor cursor = type_proxy.cursor;
  CXType type = clang_getCursorType(cursor);

  if (type.kind == CXType_Invalid) {
    return out << "nil";
  }

  out << "(cursor-type"
      << " " << repr(type);

  out << alistEntry("typedef-underlying", repr(clang_getTypedefDeclUnderlyingType(cursor)));

  int num_args = clang_Cursor_getNumArguments(cursor);
  if (num_args > 0) {
    out << " (args";
    for (int i = 0; i < num_args; ++i) {
      CXCursor arg_cursor = clang_Cursor_getArgument(cursor, i);
      out << " " << cursorRef(arg_cursor);
    }
    out << ")";
  }

  CXCursor cursor_referenced = clang_getCursorReferenced(cursor);
  if (!clang_isInvalid(clang_getCursorKind(cursor_referenced))) {
    out << alistEntry("ref", cursorRef(cursor_referenced));
  }

  CXCursor cursor_def = clang_getCursorDefinition(cursor);
  if (!clang_isInvalid(clang_getCursorKind(cursor_def))) {
    out << alistEntry("def", cursorRef(cursor_def));
  }

  if (type.kind == CXType_Overload) {
    CXCursor ref = cursor;
    int num_overloaded = clang_getNumOverloadedDecls(ref);
    // FIXME I'm not really sure what the rules are here
    if (!num_overloaded) {
      ref = cursor_referenced;
      num_overloaded = clang_getNumOverloadedDecls(ref);
    }
    if (num_overloaded > 0) {
      out << " (overloaded";
      for (int i = 0; i < num_overloaded; ++i) {
        CXCursor decl = clang_getOverloadedDecl(ref, i);
        out << " " << repr(clang_getCursorType(decl));
      }
      out << ")";
    }
  }

  // FIXME This is too recent (Dec 2014?)
  // int num_template_args = clang_Cursor_getNumTemplateArguments(cursor);
  // if (num_template_args > 0) {
  //   out << " (template-args";
  //   for (int i = 0; i < num_args; ++i) {
  //     out << " ";
  //     if (clang_Cursor_getTemplateArgumentKind(cursor, i)
  //         == CXTemplateArgumentKind_Integral) {
  //       out << clang_Cursor_getTemplateArgumentValue(cursor, i);
  //     } else {
  //       out << repr(clang_Cursor_getTemplateArgumentType(cursor, i));
  //     }
  //   }
  // }

  // out << alistEntry("num-template-args", clang_Cursor_getNumTemplateArguments(cursor));

  out << ")";
  return out;
}

std::ostream& operator<<(std::ostream& out, const cursorRef_proxy& cursorRef) {
  if (clang_isInvalid(clang_getCursorKind(cursorRef.other_cursor))) {
    out << "nil";
  } else {
    out << "(cursor-ref "
        << "#x" << std::hex << clang_hashCursor(cursorRef.other_cursor) << std::dec
        << ")";
  }
  return out;
}

std::ostream& operator<<(std::ostream& out, const comment_proxy& proxy) {
  CXString comment = clang_Cursor_getBriefCommentText(proxy.cursor);
  if (!clang_getCString(comment)) {
    CXCursor cursor = clang_getCursorDefinition(proxy.cursor);
    comment = clang_Cursor_getBriefCommentText(cursor);
  }
  if (!clang_getCString(comment)) {
    CXCursor cursor = clang_getCursorReferenced(proxy.cursor);
    comment = clang_Cursor_getBriefCommentText(cursor);
  }
  if (clang_getCString(comment)) out << repr(comment);
  else out << "nil";
  return out;
}

} // namespace detail
} // namespace sexp
