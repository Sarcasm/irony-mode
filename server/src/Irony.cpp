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

#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>
#include <vector>

namespace {

std::string cxStringToStd(CXString cxString) {
  std::string stdStr;

  if (const char *cstr = clang_getCString(cxString)) {
    stdStr = cstr;
  }

  clang_disposeString(cxString);
  return stdStr;
}

std::string diagnosticSeverityStr(const CXDiagnosticSeverity severity) {
  std::string ret;

  switch (severity) {
  case CXDiagnostic_Ignored:
    ret = "ignored";
    break;
  case CXDiagnostic_Note:
    ret = "note";
    break;
  case CXDiagnostic_Warning:
    ret = "warning";
    break;
  case CXDiagnostic_Error:
    ret = "error";
    break;
  case CXDiagnostic_Fatal:
    ret = "fatal";
    break;
  default:
    ret = "unknown";
  }

  return ret;
}

struct ParsedLocation_t {
  ParsedLocation_t() : line(0), column(0), endColumn(0), filtered(false) {}
  std::string file;
  unsigned line;
  unsigned column;
  unsigned endColumn;
  CXDiagnosticSeverity severity;
  std::string message;
  std::vector<std::string> notes;
  std::vector<std::string> hints;
  bool filtered;
};

typedef std::vector<ParsedLocation_t> ParsedLocations_t;

unsigned getLocationEndColumn(const CXTranslationUnit &tu, const CXSourceLocation& location, const unsigned startColumn) {
  unsigned endColumn(0);
  CXCursor cursor = clang_getCursor(tu, location);
  CXSourceRange range = clang_getCursorExtent(cursor);
  CXToken* tokens;
  uint numTokens;

  clang_tokenize(tu, range, &tokens, &numTokens);

  for (unsigned t = 0; t < numTokens && endColumn == 0; ++t) {
    CXSourceLocation tokenLocation = clang_getTokenLocation(tu, tokens[t]);

    if (clang_equalLocations(tokenLocation, location)) {
      const std::string name = cxStringToStd(clang_getTokenSpelling(tu, tokens[t]));
      endColumn = startColumn + name.length();
    }
  }

  clang_disposeTokens(tu, tokens, numTokens);

  return endColumn;
}

void getLocationInformation(const CXTranslationUnit &tu, const CXSourceLocation &location, std::string *file, unsigned *line, unsigned *column, unsigned *endColumn) {
  assert(location != nullptr);
  assert(line != nullptr);
  assert(column != nullptr);
  // clang_getInstantiationLocation() has been marked deprecated and
  // is aimed to be replaced by clang_getExpansionLocation().
  CXFile cxFile;
#if CINDEX_VERSION >= 6
  clang_getExpansionLocation(location, (file == nullptr ? nullptr : &cxFile), line, column, nullptr);
#else
  clang_getInstantiationLocation(location, (file == nullptr ? nullptr : &cxFile), line, column, nullptr);
#endif
  if (file != nullptr)
    *file = cxStringToStd(clang_getFileName(cxFile));
  if (endColumn != nullptr && *column > 0)
    *endColumn = getLocationEndColumn(tu, location, *column);
}

ParsedLocations_t parseDiagnostic(const CXTranslationUnit &tu, const CXDiagnostic &diagnostic) {
  ParsedLocations_t ret;
  CXSourceLocation location = clang_getDiagnosticLocation(diagnostic);

  if (!clang_equalLocations(location, clang_getNullLocation())) {
    ParsedLocation_t parsedLocation;

    getLocationInformation(tu, location, &parsedLocation.file, &parsedLocation.line, &parsedLocation.column, &parsedLocation.endColumn);

    parsedLocation.severity = clang_getDiagnosticSeverity(diagnostic);
    parsedLocation.message = cxStringToStd(clang_getDiagnosticSpelling(diagnostic));

    // Extract hints.
    // Hint ranges can be used to fine tune start/end locations.
    // Especially nice for things such as:
    // if (foo = 15) {...}
    // where only the equal sign will be marked, but one of the hints will be
    // about adding parentheses around the expression, on both sides.
    // Adjust start/end column accordingly means the entire expression
    // within the parentheses will be marked.

    const unsigned hintCount = clang_getDiagnosticNumFixIts(diagnostic);
    if (hintCount > 0) {
      CXSourceRange range;
      unsigned line(0), column(0);

      for (unsigned h = 0; h < hintCount; ++h) {
        CXString cxHint = clang_getDiagnosticFixIt(diagnostic, h, &range);
        parsedLocation.hints.emplace_back(cxStringToStd(cxHint));

        location = clang_getRangeStart(range);
        getLocationInformation(tu, location, nullptr, &line, &column, nullptr);
        if (line == parsedLocation.line && column < parsedLocation.column)
          parsedLocation.column = column;

        location = clang_getRangeEnd(range);
        getLocationInformation(tu, location, nullptr, &line, &column, nullptr);
        if (line == parsedLocation.line && column > parsedLocation.endColumn)
          parsedLocation.endColumn = column;
      }
    }

    ret.push_back(parsedLocation);

    // Clang documentation states that we don't have to release the CXDiagnosticSet
    // obtained from clang_getChildDiagnostics() with clang_diposeDiagnosticSet().
    CXDiagnosticSet children = clang_getChildDiagnostics(diagnostic);
    const unsigned childrenCount = clang_getNumDiagnosticsInSet(children);
    for (unsigned c = 0; c < childrenCount; ++c) {
      CXDiagnostic childDiagnostic = clang_getDiagnosticInSet(children, c);
      ParsedLocations_t childLocations = parseDiagnostic(tu, childDiagnostic);

      ret.reserve(ret.size() + childLocations.size());
      ret.insert(ret.end(), childLocations.begin(), childLocations.end());

      // But I suppose we _do_ have to dispose of the child diagnostic itself,
      // since the documentation for clang_getDiagnosticInSet() says it must
      // be disposed with clang_disposeDiagnostic().,
      clang_disposeDiagnostic(childDiagnostic);
    }
  }

  return ret;
}

void dumpDiagnostics(const CXTranslationUnit &tu) {
  //
  // A note on child diagnostics, "notes" and "hints" (fix-its):
  //
  // Children are the "notes". I.e. for something like:
  //
  // line 1: #ifndef TEST
  // line 2: #define TSET
  //
  // The diagnostic itself will be for line 1:
  // warning: 'TEST' is used as a header guard here, followed by #define of a different macro
  //
  // And there will be a child diagnostic for line 2:
  // note: 'TSET' is defined here; did you mean 'TEST'?
  //
  // There can also be multiple "notes". I.e. for something like:
  //
  // line 1: int foo = 15;
  // line 2: if (foo = 15) {...}
  //
  // The diagnostic itself will be for line 2:
  // warning: using the result of an assignment as a condition without parentheses
  //
  // And There will be two notes, _also_ for line 2:
  // note: place parentheses around the assignment to silence this warning
  // note: use '==' to turn this assignment into an equality comparison
  //
  // There can also be "hints" (fix-its) attached to "notes".
  // For the above header guard example, the note will be accompanied
  // by a fix-it "hint" that simply says "TEST" (without the quotation marks)
  // since that's what can replace the text with.
  //
  // We'll extract the children as separate diagnostics because that's
  // what we want for the first case. Then, we'll collapse the ones that needs
  // it for the second case. But we'll drop "hints" for "notes" because
  // they'll just look weird in the message.
  //
  // In addition, there seems to be no good way of combining multiple hints
  // into a message. Combining notes with "or" seems to work well, but needs
  // a bit more testing to see if there's cases it doesn't work for. But for
  // hints, that turns out all wrong. Consider the exmaple above with the
  // assignment that probably should have been a equality test. There will be
  // three "hints" to that, with different ranges pointing to where they should
  // go: "(", ")" and "==". Combining them togehter with "or" doesn't work since
  // the two first should belong together in a text message, but we've discarded
  // that information. So for now, just include "hints" if there's only one.
  //

  const unsigned diagnosticCount = clang_getNumDiagnostics(tu);
  ParsedLocations_t parsedLocations;
  parsedLocations.reserve(diagnosticCount);

  for (unsigned d = 0; d < diagnosticCount; ++d) {
    CXDiagnostic diagnostic = clang_getDiagnostic(tu, d);
    ParsedLocations_t diagLocations = parseDiagnostic(tu, diagnostic);
    parsedLocations.reserve(diagLocations.size());
    parsedLocations.insert(parsedLocations.end(), diagLocations.begin(), diagLocations.end());
    clang_disposeDiagnostic(diagnostic);
  }

  // Collapsing children / filtering.
  static const std::string declaredHere("' declared here");
  ParsedLocation_t* reference(nullptr);

  for (auto& current : parsedLocations) {
    if (current.severity == CXDiagnostic_Note && reference != nullptr) {
      if (current.file == reference->file && current.line == reference->line) {
        reference->notes.push_back(current.message);
        reference->hints.insert(
          reference->hints.end(), current.hints.begin(), current.hints.end());

        // Make sure fine tuned columns from hints makes it over to the reference.
        if (current.column < reference->column)
          reference->column = current.column;
        if (current.endColumn > reference->endColumn)
          reference->endColumn = current.endColumn;

        current.filtered = true;
      }
      else if (current.file != reference->file) {
        // Note might refer to other file. E.g:
        // std::tring ...
        // error: no type named 'tring' in namespace 'std'; did you mean 'string'?
        // <path>/iosfwd note: 'string' declared here
        // Skip the reference to other files.
        current.filtered = true;
      }
      else if (current.message.rfind(declaredHere) == current.message.length() - declaredHere.length()) {
        // Notes might be on the form:
        // note: '<identifier>' declared here
        // Those might refer to other files (as above), or the current file.
        // In the case of other files, Flycheck won't display them in the current buffer of course.
        // But in the case of the same file, those notes are likely to refer to a line above the
        // current error. Which looks strange, out of place and lacking context when Flycheck marks
        // them as they have no reference to the actual line where they are used.
        // Therefore, filter them out.
        current.filtered = true;
      }
    }

    if (!current.filtered)
      reference = &current;
  }

  // Removing filtered locations.
  // This is strictly speaking not necessary - they could have been skipped when dumping output.
  parsedLocations.erase(
    std::remove_if(parsedLocations.begin(), parsedLocations.end(),
                   [] (const ParsedLocation_t& parsedLocation) { return parsedLocation.filtered; }),
    parsedLocations.end());

  // Dumping output.

  std::cout << "(\n";

  for (const auto& parsedLocation : parsedLocations) {
    std::string message(parsedLocation.message);

    // Combining "notes" and "hints" into the message.
    //
    // Skipping "hints" if there are "notes" since the "notes" will
    // spell out what the "hints" show the syntax for.
    //
    // Same applies if the diagnostic itself is a "note".
    //
    // Also haven't figure out a good way to combine multiple "hints",
    // so only include "hints" if there's only one.
    //
    // And finally, if the "hint" is a spelling correction, skip it.
    // The message itself will alredy contain it.

    if (!parsedLocation.notes.empty()) {
      std::string note;
      for (const auto& n : parsedLocation.notes)
        note += (note.empty() ? "" : " or ") + n;
      message += " (note: " + note + ")";
    }
    else if (parsedLocation.hints.size() == 1 && parsedLocation.severity != CXDiagnostic_Note) {
      const std::string hint = parsedLocation.hints[0];
      const std::string didYouMean(" did you mean '" + hint + "'?");
      if (message.rfind(didYouMean) != message.length() - didYouMean.length())
        message += " (hint: " + hint + ")";
    }

    std::cout << '(' << support::quoted(parsedLocation.file)
              << ' ' << parsedLocation.line
              << ' ' << parsedLocation.column
              << ' ' << parsedLocation.endColumn
              << ' ' << diagnosticSeverityStr(parsedLocation.severity)
              << ' ' << support::quoted(message)
              << ")\n";
  }

  std::cout << ")\n";

}

} // anonymous namespace

Irony::Irony() : activeTu_(nullptr), debug_(false) {
}

void Irony::parse(const std::string &file,
                  const std::vector<std::string> &flags,
                  const std::vector<CXUnsavedFile> &unsavedFiles) {
  activeTu_ = tuManager_.parse(file, flags, unsavedFiles);
  file_ = file;
  std::cout << (activeTu_ ? "t" : "nil") << "\n";
}

void Irony::diagnostics() const {
  if (activeTu_ == nullptr) {
    std::clog << "W: diagnostics - parse wasn't called\n";

    std::cout << "nil\n";
    return;
  }

  dumpDiagnostics(activeTu_);
}

void Irony::getType(unsigned line, unsigned col) const {
  if (activeTu_ == nullptr) {
    std::clog << "W: get-type - parse wasn't called\n";

    std::cout << "nil\n";
    return;
  }

  CXFile cxFile = clang_getFile(activeTu_, file_.c_str());
  CXSourceLocation sourceLoc = clang_getLocation(activeTu_, cxFile, line, col);
  CXCursor cursor = clang_getCursor(activeTu_, sourceLoc);

  if (clang_Cursor_isNull(cursor)) {
    // TODO: "error: no type at point"?
    std::cout << "nil";
    return;
  }

  CXType cxTypes[2];
  cxTypes[0] = clang_getCursorType(cursor);
  cxTypes[1] = clang_getCanonicalType(cxTypes[0]);

  std::cout << "(";

  for (const CXType &cxType : cxTypes) {
    CXString typeDescr = clang_getTypeSpelling(cxType);
    std::string typeStr = clang_getCString(typeDescr);
    clang_disposeString(typeDescr);

    if (typeStr.empty())
      break;

    std::cout << support::quoted(typeStr) << " ";
  }

  std::cout << ")\n";
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
      CXAvailabilityKind availability =
          clang_getCompletionAvailability(candidate.CompletionString);

      unsigned priority =
          clang_getCompletionPriority(candidate.CompletionString);
      unsigned annotationStart = 0;
      bool typedTextSet = false;

      if (availability == CXAvailability_NotAccessible ||
          availability == CXAvailability_NotAvailable) {
        continue;
      }

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

void Irony::getCompileOptions(const std::string &buildDir,
                              const std::string &file) const {
#if !(HAS_COMPILATION_DATABASE)

  (void)buildDir;
  (void)file;

  std::cout << "nil\n";
  return;

#else
  CXCompilationDatabase_Error error;
  CXCompilationDatabase db =
      clang_CompilationDatabase_fromDirectory(buildDir.c_str(), &error);

  switch (error) {
  case CXCompilationDatabase_CanNotLoadDatabase:
    std::clog << "I: could not load compilation database in '" << buildDir
              << "'\n";
    std::cout << "nil\n";
    return;

  case CXCompilationDatabase_NoError:
    break;
  }

  CXCompileCommands compileCommands =
      clang_CompilationDatabase_getCompileCommands(db, file.c_str());

  std::cout << "(\n";

  for (unsigned i = 0, numCompileCommands =
                           clang_CompileCommands_getSize(compileCommands);
       i < numCompileCommands; ++i) {
    CXCompileCommand compileCommand =
        clang_CompileCommands_getCommand(compileCommands, i);

    std::cout << "("
              << "(";
    for (unsigned j = 0,
                  numArgs = clang_CompileCommand_getNumArgs(compileCommand);
         j < numArgs; ++j) {
      CXString arg = clang_CompileCommand_getArg(compileCommand, j);
      std::cout << support::quoted(clang_getCString(arg)) << " ";
      clang_disposeString(arg);
    }

    std::cout << ")"
              << " . ";

    CXString directory = clang_CompileCommand_getDirectory(compileCommand);
    std::cout << support::quoted(clang_getCString(directory));
    clang_disposeString(directory);

    std::cout << ")\n";
  }

  std::cout << ")\n";

  clang_CompileCommands_dispose(compileCommands);
  clang_CompilationDatabase_dispose(db);
#endif
}
