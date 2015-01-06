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
#include <cassert>
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

void Irony::toplevelAST(const std::string &file,
                       unsigned line,
                       unsigned col,
                       const std::vector<std::string> &flags,
                       const std::vector<CXUnsavedFile> &unsavedFiles)
{
  // FIXME Is this necessary here? Why isn't this global?
  TUManager::Settings settings;
  settings.parseTUOptions |= CXTranslationUnit_CacheCompletionResults;
#if HAS_BRIEF_COMMENTS_IN_COMPLETION
  settings.parseTUOptions |=
    CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;
#endif
  (void)tuManager_.registerSettings(settings);

  CXTranslationUnit tu
    = tuManager_.getOrCreateTU(file, flags, unsavedFiles);

  if (tu == nullptr) {
    std::cout << "nil" << std::endl;
    return;
  }

  if (debug_) {
    std::clog << "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
    dumpDiagnostics(tu);
    std::clog << "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
  }

  CXFile cxfile = clang_getFile(tu, file.c_str());
  CXSourceLocation point_loc = clang_getLocation(tu, cxfile, line, col);
  CXCursor toplevel_cursor = getToplevelCursor(tu, point_loc);

  if (clang_isTranslationUnit(clang_getCursorKind(toplevel_cursor))
      || clang_isInvalid(clang_getCursorKind(toplevel_cursor))) {
    std::cout << "nil" << std::endl;
    return;
  }

  std::cout << "(";
  astSexpPrinter(toplevel_cursor, clang_getNullCursor());

  // FIXME This doesn't really work well when the there were
  // errors: libclang can just refuse to recurse into children
  auto visitor = [](CXCursor cursor, CXCursor parent, CXClientData client_data) -> CXChildVisitResult {
    return static_cast<Irony*>(client_data)->astSexpPrinter(cursor, parent);
  };
  clang_visitChildren(toplevel_cursor, visitor, this);

  std::cout << ")" << std::endl;
}

CXChildVisitResult Irony::astSexpPrinter(CXCursor cursor, CXCursor parent) {
  unsigned start_offset, end_offset, parent_offset;

  CXSourceRange range = clang_getCursorExtent(cursor),
    parent_range = clang_getCursorExtent(parent);
  CXSourceLocation start = clang_getRangeStart(range),
    end = clang_getRangeEnd(range),
    parent_start = clang_getRangeStart(parent_range);
  clang_getSpellingLocation(start, nullptr, nullptr, nullptr, &start_offset);
  clang_getSpellingLocation(end, nullptr, nullptr, nullptr, &end_offset);
  clang_getSpellingLocation(parent_start, nullptr, nullptr, nullptr, &parent_offset);

  CXCursorKind kind = clang_getCursorKind(cursor);
  CXCursorKind parent_kind = clang_getCursorKind(parent);
  bool parent_valid =
    !clang_isTranslationUnit(parent_kind) && !clang_isInvalid(parent_kind);

  if (debug_) std::cout << "\nastVisitor ⇒ ";

  // NOTE See cl-defstruct in irony-ast.el for the order of elements
  if (parent_valid) std::cout << " "; // first space
  std::cout
    << "[cl-struct-irony-ast"
    << " " << repr(kind)
    << " " << (1 + start_offset) << " " << (1 + end_offset)
    << " " << sexp::cursorRef(parent)
    << " " << sexp::cursorRef(getCursorFirstChild(cursor))
    << " " << sexp::cursorRef(getCursorPrev(cursor, parent))
    << " " << sexp::cursorRef(getCursorNext(cursor, parent))
    << " #x" << std::hex << clang_hashCursor(cursor) << std::dec
    << " " << sexp::type(cursor);

  // Spelling of the thing this cursor references
  // TODO Is there a difference between spelling and display name?
  // std::cout << " " << repr(clang_getCursorSpelling(cursor));
  std::cout << " " << repr(clang_getCursorDisplayName(cursor));

  std::cout << " " << sexp::comment(cursor);
  std::cout << "]";

  return CXChildVisit_Recurse;
}

CXCursor Irony::getToplevelCursor(CXTranslationUnit tu, CXSourceLocation sloc)
{
  CXCursor cursor = clang_getCursor(tu, sloc);
  CXCursorKind kind = clang_getCursorKind(cursor);
  while(! clang_isTranslationUnit(kind) && ! clang_isInvalid(kind)) {
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


// Utility functions

long cursorOffset(CXCursor cursor) {
  unsigned offset;
  CXSourceRange range = clang_getCursorExtent(cursor);
  CXSourceLocation sloc = clang_getRangeStart(range);
  clang_getSpellingLocation(sloc, nullptr, nullptr, nullptr, &offset);
  return (long)offset;
}

CXCursor getCursorParent(CXCursor cursor) {
  CXCursor parent = clang_getCursorLexicalParent(cursor);
  if (clang_isInvalid(clang_getCursorKind(parent))) {
    parent = clang_getCursorSemanticParent(cursor);
  }
  return parent;
}

CXCursor getCursorFirstChild(CXCursor cursor) {
  CXCursor first_child = clang_getNullCursor();
  auto visitor =
    [](CXCursor current, CXCursor, CXClientData client_data) -> CXChildVisitResult {
      *static_cast<CXCursor*>(client_data) = current;
      return CXChildVisit_Break;
    };
  clang_visitChildren(cursor, visitor, &first_child);
  return first_child;
}

CXCursor getCursorNext(CXCursor cursor, CXCursor parent) {
  struct data_t {
    bool visited;
    const CXCursor cursor;
    CXCursor sibling;
  } data { false, cursor, clang_getNullCursor() };
  auto visitor =
    [](CXCursor current, CXCursor, CXClientData client_data)
    -> CXChildVisitResult
    {
      data_t& data = *static_cast<data_t*>(client_data);
      if (data.visited) {
        data.sibling = current;
        return CXChildVisit_Break;
      }
      data.visited = clang_equalCursors(current, data.cursor);
      return CXChildVisit_Continue;
    };
  clang_visitChildren(parent, visitor, &data);
  assert(!clang_equalCursors(cursor, data.sibling));
  return data.sibling;
}

CXCursor getCursorPrev(CXCursor cursor, CXCursor parent) {
  if (clang_isInvalid(clang_getCursorKind(cursor))
      || clang_isInvalid(clang_getCursorKind(parent)))
    return clang_getNullCursor();
  struct data_t {
    const CXCursor cursor;
    CXCursor sibling;
  } data { cursor, clang_getNullCursor() };
  auto visitor =
    [](CXCursor current, CXCursor, CXClientData client_data)
    -> CXChildVisitResult
    {
      data_t& data = *static_cast<data_t*>(client_data);
      if (clang_equalCursors(current, data.cursor))
        return CXChildVisit_Break;
      data.sibling = current;
      return CXChildVisit_Continue;
    };
  auto result = clang_visitChildren(parent, visitor, &data);
  return result ? data.sibling : clang_getNullCursor();
}
