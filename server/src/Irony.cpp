/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief irony-server "API" definitions.
 *
 * \sa Irony.h for more information.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#include "Irony.h"

#include "support/iomanip_quoted.h"
#include "support/Sexp.h"
using sexp::repr;

#include <algorithm>
#include <array>
#include <iostream>
#include <sstream>
#include <stdexcept>

#if defined(CINDEX_VERSION_MAJOR) && defined(CINDEX_VERSION_MINOR) &&          \
    (CINDEX_VERSION_MAJOR > 0 || CINDEX_VERSION_MINOR >= 6)
#define HAS_BRIEF_COMMENTS_IN_COMPLETION 1
#else
#define HAS_BRIEF_COMMENTS_IN_COMPLETION 0
#endif

static std::string cxStringToStd(CXString cxString) {
  std::string stdStr;

  if (const char *cstr = clang_getCString(cxString)) {
    stdStr = cstr;
  }

  clang_disposeString(cxString);
  return stdStr;
}

Irony::Irony() : debug_(false) {
}

static const char *diagnosticSeverity(CXDiagnostic diagnostic) {
  switch (clang_getDiagnosticSeverity(diagnostic)) {
  case CXDiagnostic_Ignored:
    return "ignored";
  case CXDiagnostic_Note:
    return "note";
  case CXDiagnostic_Warning:
    return "warning";
  case CXDiagnostic_Error:
    return "error";
  case CXDiagnostic_Fatal:
    return "fatal";
  }

  return "unknown";
}

static void dumpDiagnostics(const CXTranslationUnit &tu) {
  std::cout << "(\n";

  std::string file;

  for (unsigned i = 0, diagnosticCount = clang_getNumDiagnostics(tu);
       i < diagnosticCount;
       ++i) {
    CXDiagnostic diagnostic = clang_getDiagnostic(tu, i);

    CXSourceLocation location = clang_getDiagnosticLocation(diagnostic);

    unsigned line, column, offset;
    if (clang_equalLocations(location, clang_getNullLocation())) {
      file.clear();
      line = 0;
      column = 0;
      offset = 0;
    } else {
      CXFile cxFile;

// clang_getInstantiationLocation() has been marked deprecated and
// is aimed to be replaced by clang_getExpansionLocation().
#if defined(CINDEX_VERSION_MAJOR) && defined(CINDEX_VERSION_MINOR) &&          \
    (CINDEX_VERSION_MAJOR > 0 || CINDEX_VERSION_MINOR >= 6)
      clang_getExpansionLocation(location, &cxFile, &line, &column, &offset);
#else
      clang_getInstantiationLocation(location, &cxFile, &line, &column, &offset);
#endif

      file = cxStringToStd(clang_getFileName(cxFile));
    }

    const char *severity = diagnosticSeverity(diagnostic);

    std::string message =
        cxStringToStd(clang_getDiagnosticSpelling(diagnostic));

    std::cout << '(' << support::quoted(file)    //
              << ' ' << line                     //
              << ' ' << column                   //
              << ' ' << offset                   //
              << ' ' << severity                 //
              << ' ' << support::quoted(message) //
              << ")\n";

    clang_disposeDiagnostic(diagnostic);
  }

  std::cout << ")\n";
}

void Irony::diagnostics(const std::string &file,
                        const std::vector<std::string> &flags,
                        const std::vector<CXUnsavedFile> &unsavedFiles) {
  CXTranslationUnit tu = tuManager_.parse(file, flags, unsavedFiles);

  if (tu == nullptr) {
    std::cout << "nil\n";
    return;
  }

  dumpDiagnostics(tu);
}

namespace {

class CompletionChunk {
public:
  explicit CompletionChunk(CXCompletionString completionString)
    : completionString_(completionString)
    , numChunks_(clang_getNumCompletionChunks(completionString_))
    , chunkIdx_(0) {
  }

  bool hasNext() const {
    return chunkIdx_ < numChunks_;
  }

  void next() {
    if (!hasNext()) {
      assert(0 && "out of range completion chunk");
      abort();
    }

    ++chunkIdx_;
  }

  CXCompletionChunkKind kind() const {
    return clang_getCompletionChunkKind(completionString_, chunkIdx_);
  }

  std::string text() const {
    return cxStringToStd(
        clang_getCompletionChunkText(completionString_, chunkIdx_));
  }

private:
  CXCompletionString completionString_;
  unsigned int numChunks_;
  unsigned chunkIdx_;
};

} // unnamed namespace

void Irony::complete(const std::string &file,
                     unsigned line,
                     unsigned col,
                     const std::vector<std::string> &flags,
                     const std::vector<CXUnsavedFile> &unsavedFiles) {
  // Register the settings the first time, to enable optimization of code
  // completion and request brief comments if available.
  TUManager::Settings settings;

  settings.parseTUOptions |= CXTranslationUnit_CacheCompletionResults;

#if HAS_BRIEF_COMMENTS_IN_COMPLETION
  settings.parseTUOptions |=
      CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;
#endif

  (void)tuManager_.registerSettings(settings);

  CXTranslationUnit tu = tuManager_.getOrCreateTU(file, flags, unsavedFiles);

  if (tu == nullptr) {
    std::cout << "nil\n";
    return;
  }

  if (CXCodeCompleteResults *completions =
          clang_codeCompleteAt(tu,
                               file.c_str(),
                               line,
                               col,
                               const_cast<CXUnsavedFile *>(unsavedFiles.data()),
                               unsavedFiles.size(),
                               (clang_defaultCodeCompleteOptions() &
                                ~CXCodeComplete_IncludeCodePatterns)
#if HAS_BRIEF_COMMENTS_IN_COMPLETION
                                   |
                                   CXCodeComplete_IncludeBriefComments
#endif
                               )) {

    if (debug_) {
      unsigned numDiags = clang_codeCompleteGetNumDiagnostics(completions);
      std::clog << "debug: complete: " << numDiags << " diagnostic(s)\n";
      for (unsigned i = 0; i < numDiags; ++i) {
        CXDiagnostic diagnostic =
            clang_codeCompleteGetDiagnostic(completions, i);
        CXString s = clang_formatDiagnostic(
            diagnostic, clang_defaultDiagnosticDisplayOptions());

        std::clog << clang_getCString(s) << std::endl;
        clang_disposeString(s);
        clang_disposeDiagnostic(diagnostic);
      }
    }

    clang_sortCodeCompletionResults(completions->Results,
                                    completions->NumResults);

    std::cout << "(\n";

    // re-use the same buffers to avoid unnecessary allocations
    std::string typedtext, brief, resultType, prototype, postCompCar;
    std::vector<unsigned> postCompCdr;

    for (unsigned i = 0; i < completions->NumResults; ++i) {
      CXCompletionResult candidate = completions->Results[i];

      unsigned priority =
          clang_getCompletionPriority(candidate.CompletionString);
      unsigned annotationStart = 0;
      bool typedTextSet = false;

      typedtext.clear();
      brief.clear();
      resultType.clear();
      prototype.clear();
      postCompCar.clear();
      postCompCdr.clear();

      for (CompletionChunk chunk(candidate.CompletionString); chunk.hasNext();
           chunk.next()) {
        char ch = 0;

        auto chunkKind = chunk.kind();

        switch (chunkKind) {
        case CXCompletionChunk_ResultType:
          resultType = chunk.text();
          break;

        case CXCompletionChunk_TypedText:
        case CXCompletionChunk_Text:
        case CXCompletionChunk_Placeholder:
        case CXCompletionChunk_Informative:
        case CXCompletionChunk_CurrentParameter:
          prototype += chunk.text();
          break;

        case CXCompletionChunk_LeftParen:       ch = '(';  break;
        case CXCompletionChunk_RightParen:      ch = ')';  break;
        case CXCompletionChunk_LeftBracket:     ch = '[';  break;
        case CXCompletionChunk_RightBracket:    ch = ']';  break;
        case CXCompletionChunk_LeftBrace:       ch = '{';  break;
        case CXCompletionChunk_RightBrace:      ch = '}';  break;
        case CXCompletionChunk_LeftAngle:       ch = '<';  break;
        case CXCompletionChunk_RightAngle:      ch = '>';  break;
        case CXCompletionChunk_Comma:           ch = ',';  break;
        case CXCompletionChunk_Colon:           ch = ':';  break;
        case CXCompletionChunk_SemiColon:       ch = ';';  break;
        case CXCompletionChunk_Equal:           ch = '=';  break;
        case CXCompletionChunk_HorizontalSpace: ch = ' ';  break;
        case CXCompletionChunk_VerticalSpace:   ch = '\n'; break;

        case CXCompletionChunk_Optional:
          // ignored for now
          break;
        }

        if (ch != 0) {
          prototype += ch;
          // commas look better followed by a space
          if (ch == ',') {
            prototype += ' ';
          }
        }

        if (typedTextSet) {
          if (ch != 0) {
            postCompCar += ch;
            if (ch == ',') {
              postCompCar += ' ';
            }
          } else if (chunkKind == CXCompletionChunk_Text ||
                     chunkKind == CXCompletionChunk_TypedText) {
            postCompCar += chunk.text();
          } else if (chunkKind == CXCompletionChunk_Placeholder ||
                     chunkKind == CXCompletionChunk_CurrentParameter) {
            postCompCdr.push_back(postCompCar.size());
            postCompCar += chunk.text();
            postCompCdr.push_back(postCompCar.size());
          }
        }

        // Consider only the first typed text. The CXCompletionChunk_TypedText
        // doc suggests that exactly one typed text will be given but at least
        // in Objective-C it seems that more than one can appear, see:
        // https://github.com/Sarcasm/irony-mode/pull/78#issuecomment-37115538
        if (chunkKind == CXCompletionChunk_TypedText && !typedTextSet) {
          typedtext = chunk.text();
          // annotation is what comes after the typedtext
          annotationStart = prototype.size();
          typedTextSet = true;
        }
      }

#if HAS_BRIEF_COMMENTS_IN_COMPLETION
      brief = cxStringToStd(
          clang_getCompletionBriefComment(candidate.CompletionString));
#endif

      // see irony-completion.el#irony-completion-candidates
      std::cout << '(' << support::quoted(typedtext)  //
                << ' ' << priority                    //
                << ' ' << support::quoted(resultType) //
                << ' ' << support::quoted(brief)      //
                << ' ' << support::quoted(prototype)  //
                << ' ' << annotationStart             //
                << " (" << support::quoted(postCompCar);
      for (unsigned index : postCompCdr)
        std::cout << ' ' << index;
      std::cout << ")"
                << ")\n";
    }

    clang_disposeCodeCompleteResults(completions);
    std::cout << ")\n";
  }
}

unsigned getOffset(CXSourceLocation loc) {
  unsigned offset, line, col;
  clang_getSpellingLocation(loc, nullptr, &line, &col, &offset);
  return offset;
}

unsigned getOffset(CXCursor cursor) {
  CXSourceLocation sloc = clang_getCursorLocation(cursor);
  return getOffset(sloc);
}

CXCursor cursorParent(CXCursor cursor) {
  CXCursor parent = clang_getCursorLexicalParent(cursor);
  if (clang_isInvalid(clang_getCursorKind(parent))) {
    parent = clang_getCursorSemanticParent(cursor);
  }
  return parent;
}

/// Whether cursor is suitable for having its information being shown
/// to the user.
bool isCursorPrintable(CXCursor cursor) {
  CXType type = clang_getCursorType(cursor);
  bool valid = !clang_isInvalid(cursor.kind) &&
               !clang_isUnexposed(cursor.kind) &&
               !clang_isTranslationUnit(cursor.kind);
  // Unexposed types are allowed (common in C++)
  bool validType = type.kind != CXType_Invalid;
  bool isLiteral = cursor.kind == CXCursor_IntegerLiteral ||
                   cursor.kind == CXCursor_FloatingLiteral ||
                   cursor.kind == CXCursor_ImaginaryLiteral ||
                   cursor.kind == CXCursor_StringLiteral ||
                   cursor.kind == CXCursor_CharacterLiteral;
  return valid && validType && !isLiteral;
}

CXCursor getToplevelCursor(CXTranslationUnit tu, CXSourceLocation sloc) {
  CXCursor cursor = clang_getCursor(tu, sloc);
  CXCursorKind kind = clang_getCursorKind(cursor);
  while (!clang_isTranslationUnit(kind) && !clang_isInvalid(kind)) {
    CXCursor parent = clang_getCursorLexicalParent(cursor);
    if (clang_isInvalid(clang_getCursorKind(parent))) {
      parent = clang_getCursorSemanticParent(cursor);
    }
    if (clang_isTranslationUnit(clang_getCursorKind(parent))) {
      break;
    }
    cursor = parent;
    kind = clang_getCursorKind(cursor);
  }
  return cursor;
}

CXCursor getCursorFirstChild(CXCursor cursor) {
  CXCursor first_child = clang_getNullCursor();
  auto visitor = [](CXCursor current, CXCursor, CXClientData client_data)
                     -> CXChildVisitResult {
    *static_cast<CXCursor *>(client_data) = current;
    return CXChildVisit_Break;
  };
  clang_visitChildren(cursor, visitor, &first_child);
  return first_child;
}

/// Wrapper around clang_getCursorExtent
struct Range {
  Range(CXCursor cursor)
    : range(clang_getCursorExtent(cursor))
    , start(clang_getRangeStart(range))
    , end(clang_getRangeEnd(range))
    , start_offset(getOffset(start))
    , end_offset(getOffset(end)) {
  }
  bool contains(unsigned offset) {
    return start_offset <= offset && offset < end_offset;
  }
  bool contains(unsigned a, unsigned b) {
    return start_offset <= std::min(a, b) && std::max(a, b) <= end_offset;
  }
  CXSourceRange range;
  CXSourceLocation start, end;
  unsigned start_offset, end_offset;
};

struct exprtypeAlist {
  exprtypeAlist(const CXCursor &cursor) : cursor(cursor) {
  }
  const CXCursor &cursor;
};

struct typeAlist {
  typeAlist(const CXType &type) : type(type) {
  }
  const CXType &type;
};

std::ostream &operator<<(std::ostream &out, const typeAlist &proxy) {
  CXType type = proxy.type;
  if (type.kind == CXType_Invalid)
    return out << "nil";
  out << "(";
  if (type.kind != CXType_Unexposed)
    out << sexp::alistEntry("kind", repr(type.kind));
  out << sexp::alistEntry("spelling", repr(clang_getTypeSpelling(type)));

  int numargs = clang_getNumArgTypes(type);
  if (numargs >= 0) {
    out << " (args";
    for (int i = 0; i < numargs; ++i) {
      CXType arg = clang_getArgType(type, i);
      out << sexp::alistEntry("type", typeAlist(arg));
    }
    out << ")";
  }

  if (clang_isFunctionTypeVariadic(type)) {
    out << " (variadic . t)";
  }

  return out << ")";
}

std::ostream& operator<<(std::ostream& out, const exprtypeAlist &proxy) {
  CXCursor cursor = proxy.cursor;

  /* Examples of what might be printed (from test.cc)

FIXME More robust examples and testing

(exprtype (bounds 832 836) (kind . CallExpr) (type (kind . Dependent) (spelling . "<dependent type>")) (call (bounds 832 833) (kind . OverloadedDeclRef) (spelling . "h") (overloaded (completion (comment . "docstring for h(X).") (priority . 50) (chunks (ResultType . "X") "h(" (Placeholder . "const X &x")")")) (completion (comment . "docstring for h<T>") (priority . 50) (chunks (ResultType . "T") "h(" (Placeholder . "const T &x")")")))) (args (834 . 835)))

(exprtype (bounds 834 835) (kind . DeclRefExpr) (spelling . "t") (type (spelling . "T")) (completion (priority . 50) (chunks (ResultType . "T") "t")))

(exprtype (bounds 892 902) (kind . ParmDecl) (spelling . "y") (type (kind . LValueReference) (spelling . "const Y &")) (completion (priority . 50) (chunks (ResultType . "const Y &") "y")))

(exprtype (bounds 1287 1291) (kind . CallExpr) (spelling . "f") (type (kind . Void) (spelling . "void")) (completion (comment . "docstring for f") (priority . 50) (chunks (ResultType . "void") "f(" (Placeholder . "string x") ")")) (call (bounds 1287 1288) (kind . DeclRefExpr) (spelling . "f") (type (kind . FunctionProto) (spelling . "void (string)") (args (type (kind . Typedef) (spelling . "string")))) (completion (comment . "docstring for f") (priority . 50) (chunks (ResultType . "void") "f(" (Placeholder . "string x") ")")) (comment . "docstring for f")) (args (1289 . 1290)) (comment . "docstring for f"))

(exprtype (bounds 1289 1290) (kind . DeclRefExpr) (spelling . "x") (type (kind . Typedef) (spelling . "string")) (completion (priority . 50) (chunks (ResultType . "string") "x")))

NOTE: no arguments here despite this being a CallExpr
(exprtype (bounds 1205 1227) (kind . CallExpr) (spelling . "vector") (type (spelling . "vector<float>")) (completion (parent . "std::__1::vector") (priority . 50) (chunks "vector(" (Placeholder . "size_type __n") "," (Placeholder . "const_reference __x") ")")) (call (bounds 1205 1211) (kind . TemplateRef) (spelling . "vector") (completion (parent . "std::__1") (priority . 50) (chunks "vector<" (Placeholder . "class _Tp") (Optional . (completion (priority . 0) (chunks "," (Placeholder . "class _Allocator")))) ">"))))

   */

  Range range{cursor};
  CXType type = clang_getCursorType(cursor);

  // This is a long alist of essentially arbitrary elements.
  if (!clang_isInvalid(cursor.kind))
    out << " (bounds " << range.start_offset << " " << range.end_offset << ")";
  out << sexp::alistEntry("kind", repr(cursor.kind))
      << sexp::alistEntry("spelling", repr(clang_getCursorSpelling(cursor)))
      << sexp::alistEntry("type", typeAlist(type));

  // Find a suitable completion string
  {
    CXCompletionString completion =
        clang_getCursorCompletionString(clang_getCursorDefinition(cursor));
    if (!completion)
      completion =
          clang_getCursorCompletionString(clang_getCursorReferenced(cursor));
    if (!completion)
      completion = clang_getCursorCompletionString(cursor);
    if (completion)
      out << " " << sexp::completion(completion);
  }

  // If cursor is a call to a function, we show the type of the
  // function as well. Then the type is the return type.
  if (cursor.kind == CXCursor_CallExpr) {
    // Find the first child, which should be the function
    // FIXME Is this true for obj-c? I'm not familiar with it.
    CXCursor funref = cursor;
    for (CXCursor cursor_child = funref; !clang_isInvalid(cursor_child.kind);
         funref = cursor_child, cursor_child = getCursorFirstChild(funref))
      ;

    out << " (call" << exprtypeAlist(funref) << ")";
  }

  // FIXME Constructor arguments don't seem to appear here.
  // Are they of kind CallExpr?
  int numargs = clang_Cursor_getNumArguments(cursor);
  if (numargs >= 0) {
    out << " (args";
    for (int i = 0; i < numargs; ++i) {
      CXCursor arg = clang_Cursor_getArgument(cursor, i);
      Range range{arg};
      out << " (" << range.start_offset << " . " << range.end_offset << ")";
    }
    out << ")";
  }

  out << sexp::alistEntry("comment", sexp::comment(cursor));

  // Overloaded decl ref
  {
    CXCursor ref = cursor;
    int num_overloaded = clang_getNumOverloadedDecls(ref);
    if (!num_overloaded) {
      ref = clang_getCursorReferenced(cursor);
      num_overloaded = clang_getNumOverloadedDecls(ref);
    }
    if (num_overloaded > 0) {
      out << " (overloaded";
      for (int i = 0; i < num_overloaded; ++i) {
        CXCursor decl = clang_getOverloadedDecl(ref, i);
        out << " " << sexp::completion(decl);
      }
      out << ")";
    }
  }

  return out;
}

void Irony::exprtype(const std::string &file,
                    unsigned start_offset,
                    unsigned end_offset,
                    const std::vector<std::string> &flags,
                    const std::vector<CXUnsavedFile> &unsavedFiles) {
  // NOTE Duplicate from complete(..) above
  TUManager::Settings settings;
  settings.parseTUOptions |= CXTranslationUnit_CacheCompletionResults;
#if HAS_BRIEF_COMMENTS_IN_COMPLETION
  settings.parseTUOptions |=
      CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;
#endif
  (void)tuManager_.registerSettings(settings);
  CXTranslationUnit tu = tuManager_.getOrCreateTU(file, flags, unsavedFiles);

  if (!tu) {
    std::cout << "nil\n";
    return;
  }

  // if (true || debug_) {
  //   std::cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
  //   dumpDiagnostics(tu);
  //   std::cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
  // }

  if (end_offset < start_offset)
    end_offset = start_offset;

  CXFile cxfile = clang_getFile(tu, file.c_str());
  CXSourceLocation
    start_loc = clang_getLocationForOffset(tu, cxfile, start_offset);

  CXCursor cursor = clang_getCursor(tu, start_loc);

  /* The lexical parent of a cursor doesn't seem to be its parent in
  the AST, which is what we want. So go to the toplevel cursor, just
  below the translation unit, and walk down looking for printable
  cursors covering the region. */
  if (!isCursorPrintable(cursor) ||
      !Range(cursor).contains(start_offset, end_offset)) {
    cursor = getToplevelCursor(tu, start_loc);
    struct data_t {
      const unsigned start_offset, end_offset;
      CXCursor cursor;
    } data{start_offset, end_offset, clang_getNullCursor()};
    auto visitor = [](CXCursor current, CXCursor, CXClientData client_data)
                       -> CXChildVisitResult {
      Range range{current};
      data_t &data = *static_cast<data_t *>(client_data);
      if (range.contains(data.start_offset, data.end_offset)) {
        if (isCursorPrintable(current))
          data.cursor = current;
        return CXChildVisit_Recurse;
      }
      return range.start_offset >= data.end_offset ? CXChildVisit_Break
                                                   : CXChildVisit_Continue;
    };
    clang_visitChildren(cursor, visitor, &data);
    if (!clang_Cursor_isNull(data.cursor))
      cursor = data.cursor;
  }

  std::cout << "(exprtype";
  unsigned numdiag = clang_getNumDiagnostics(tu);
  if (numdiag)
    std::cout << sexp::alistEntry("diagnostics", numdiag);
  std::cout << exprtypeAlist(cursor) << ")" << std::endl;
}
