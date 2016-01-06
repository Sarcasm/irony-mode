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

static std::string cxStringToStd(CXString cxString) {
  std::string stdStr;

  if (const char *cstr = clang_getCString(cxString)) {
    stdStr = cstr;
  }

  clang_disposeString(cxString);
  return stdStr;
}

Irony::Irony() : activeTu_(nullptr), debug_(false) {
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
#if CINDEX_VERSION >= 6
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

void Irony::parse(const std::string &file,
                  const std::vector<std::string> &flags,
                  const std::vector<CXUnsavedFile> &unsavedFiles) {
  activeTu_ = tuManager_.parse(file, flags, unsavedFiles);

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

#ifdef HAS_BOOST_FILESYSTEM

static void printCompileCommand(const CompileCommand &cmd) {
  std::cout << "( (";

  for (const std::string &cmdArg : cmd.cmd_)
    std::cout << support::quoted(cmdArg) << " ";

  std::cout << ") . " << support::quoted(cmd.dir_) << ")\n";
}

void Irony::hasCompilationDatabase() {
  std::cout << "t\n";
}

void Irony::getCompileOptions(const std::string &databaseFile,
                              const std::string &file) {
  database.readOrUpdateDatabase(databaseFile);
  std::vector<const CompileCommand*> cmds = database.getCommands(file);

  std::cout << "(\n";
  for (const CompileCommand *cmd : cmds)
    printCompileCommand(*cmd);
  std::cout << ")\n";
}

void Irony::guessCompileOptions(const std::string &databaseFile,
                                const std::string &srcFile) {
  database.readOrUpdateDatabase(databaseFile);

  std::pair<const std::string *, const CompileCommand *> cmdPair =
      database.guessCommand(srcFile);

  // TODO: Assert here when ready
  if (!cmdPair.second) {
    std::clog << "I: Couldn't guess compilation command for file " << srcFile
              << ".\n";
    std::cout << "nil\n";
    return;
  }

  if (debug_)
    std::clog << "I: Using compilation command for " << *cmdPair.first << "\n";

  std::cout << "( " << support::quoted(*cmdPair.first) << " . ";
  printCompileCommand(*cmdPair.second);
  std::cout << ")\n";
}

#else // HAS_BOOST_FILESYSTEM

void Irony::hasCompilationDatabase() {
  std::cout << "nil\n";
}

void Irony::getCompileOptions(const std::string &databaseFile,
                              const std::string &file) {
  std::cout << "nil\n";
}

void Irony::guessCompileOptions(const std::string &databaseFile,
                                const std::string &srcFile) {
  std::cout << "nil\n";
}

#endif // HAS_BOOST_FILESYSTEM
