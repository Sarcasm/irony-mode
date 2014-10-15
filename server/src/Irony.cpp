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

namespace {

std::string cxStringToStd(CXString cxString) {
  std::string stdStr;

  if (const char *cstr = clang_getCString(cxString)) {
    stdStr = cstr;
  }

  clang_disposeString(cxString);
  return stdStr;
}

} // unnamed namespace

Irony::Irony() : debug_(false) {
}

void Irony::check(const std::string &file,
                  const std::vector<std::string> &flags,
                  const std::vector<CXUnsavedFile> &unsavedFiles) {
  std::cout << "(";

  unsigned numDiag = 0;
  int fatals = 0;
  int errors = 0;
  int warnings = 0;

  CXTranslationUnit tu = tuManager_.parse(file, flags, unsavedFiles);

  if (tu) {
    numDiag = clang_getNumDiagnostics(tu);
  } else {
    fatals = 1;
  }

  for (unsigned i = 0; i < numDiag; ++i) {
    CXDiagnostic diagnostic = clang_getDiagnostic(tu, i);

    switch (clang_getDiagnosticSeverity(diagnostic)) {
    case CXDiagnostic_Fatal:
      fatals++;
      break;

    case CXDiagnostic_Error:
      errors++;
      break;

    case CXDiagnostic_Warning:
      warnings++;
      break;

    default:
      break;
    }

    clang_disposeDiagnostic(diagnostic);
  }

  if (fatals > 0)
    std::cout << " :fatals " << fatals;

  if (errors > 0)
    std::cout << " :errors " << errors;

  if (warnings > 0)
    std::cout << " :warnings " << warnings;

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
