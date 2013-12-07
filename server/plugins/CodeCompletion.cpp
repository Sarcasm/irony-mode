/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief Completion plugin implementation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "CodeCompletion.h"

#include "ClangString.h"

#include "util/arraysize.h"

#include <algorithm>
#include <cstddef>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

class CompletionChunk {
public:
  explicit CompletionChunk(CXCompletionString completionString)
    : completionString_(completionString)
    , numChunks_(clang_getNumCompletionChunks(completionString_))
    , chunkIdx_(0) {
  }

  CXCompletionChunkKind kind() const {
    return clang_getCompletionChunkKind(completionString_, chunkIdx_);
  }

  std::string escapedContent() const {
    return ClangString(text(), ClangString::AddQuotes).asString();
  }

  std::string escapedUnquotedContent() const {
    return ClangString(text(), ClangString::Escape).asString();
  }

  CompletionChunk optionalChunks() const {
    CXCompletionString completionString =
        clang_getCompletionChunkCompletionString(completionString_, chunkIdx_);

    return CompletionChunk(completionString);
  }

  bool hasNext() const {
    return chunkIdx_ < numChunks_;
  }

  void next() {
    if (!hasNext()) {
      throw std::out_of_range("out of range completion chunk");
    }

    chunkIdx_++;
  }

private:
  CXString text() const {
    return clang_getCompletionChunkText(completionString_, chunkIdx_);
  }

private:
  CXCompletionString completionString_;
  unsigned int numChunks_;
  unsigned chunkIdx_;
};

class Candidate {
public:
  explicit Candidate(const CXCompletionResult &completionResult)
    : completionResult_(completionResult), priority_(-1), typedText_() {
  }

  Candidate() : completionResult_(), priority_(-1), typedText_() {
  }

  int priority() const {
    if (priority_ == -1) {
      priority_ =
          clang_getCompletionPriority(completionResult_.CompletionString);
    }
    return priority_;
  }

  const std::string &getTypedText() const {
    if (typedText_.empty()) {
      for (CompletionChunk chunk = firstChunk(); chunk.hasNext();
           chunk.next()) {

        if (chunk.kind() == CXCompletionChunk_TypedText) {
          typedText_ = chunk.escapedContent();
          break;
        }
      }
    }

    return typedText_;
  }

  /// \brief Generate a short documentation for the candidate. Contains at least
  ///        the prototype of the function and if available the brief comment.
  const std::string &briefDoc() const {
    if (briefDoc_.empty()) {
      std::ostringstream os;

      os << '"';
      formatCompletionChunks(firstChunk(), os);

#if defined(CINDEX_VERSION_MAJOR) && defined(CINDEX_VERSION_MINOR) &&          \
    (CINDEX_VERSION_MAJOR > 0 || CINDEX_VERSION_MINOR >= 6)
      CXString brief =
          clang_getCompletionBriefComment(completionResult_.CompletionString);
      ClangString briefStr(brief, ClangString::Escape);

      if (!briefStr.isNull())
        os << "\n\n" << briefStr.asString();
#endif

      os << '"';
      briefDoc_ = os.str();
    }
    return briefDoc_;
  }

  void formatCompletionChunks(const CompletionChunk &start,
                              std::ostream &os) const {
    for (CompletionChunk chunk(start); chunk.hasNext(); chunk.next()) {
      switch (chunk.kind()) {
      case CXCompletionChunk_ResultType:
        os << chunk.escapedUnquotedContent() << " ";
        break;

      case CXCompletionChunk_TypedText:
      case CXCompletionChunk_Placeholder:
      case CXCompletionChunk_Text:
      case CXCompletionChunk_Informative:
      case CXCompletionChunk_CurrentParameter:
        os << chunk.escapedUnquotedContent();
        break;

      case CXCompletionChunk_LeftParen:       os << '(';  break;
      case CXCompletionChunk_RightParen:      os << ')';  break;
      case CXCompletionChunk_LeftBracket:     os << '[';  break;
      case CXCompletionChunk_RightBracket:    os << ']';  break;
      case CXCompletionChunk_LeftBrace:       os << '{';  break;
      case CXCompletionChunk_RightBrace:      os << '}';  break;
      case CXCompletionChunk_LeftAngle:       os << '<';  break;
      case CXCompletionChunk_RightAngle:      os << '>';  break;
      case CXCompletionChunk_Comma:           os << ", ";  break;
      case CXCompletionChunk_Colon:           os << ':';  break;
      case CXCompletionChunk_SemiColon:       os << ';';  break;
      case CXCompletionChunk_Equal:           os << '=';  break;
      case CXCompletionChunk_HorizontalSpace: os << ' ';  break;
      case CXCompletionChunk_VerticalSpace:   os << '\n'; break;

      case CXCompletionChunk_Optional:
        os << '[';
        formatCompletionChunks(chunk.optionalChunks(), os);
        os << ']';
        break;

      default:
        break;
      }
    }
  }

  CompletionChunk firstChunk() const {
    return CompletionChunk(completionResult_.CompletionString);
  }

  void setCompletionResult(const CXCompletionResult &completionResult) {
    completionResult_ = completionResult;
    priority_ = -1;
  }

  bool operator<(const Candidate &rhs) const {
    const std::string &a = getTypedText();
    const std::string &b = rhs.getTypedText();

    for (std::string::size_type i = 0, e = std::min(a.length(), b.length());
         i != e;
         ++i) {
      const char low_a = std::tolower(a[i]);
      const char low_b = std::tolower(b[i]);

      // compare lower first
      if (low_a != low_b)
        return low_a < low_b;
    }

    if (a.length() == b.length()) {
      // compare case sensitive so similar cases are ordered together
      return std::string::traits_type::compare(a.data(), b.data(), a.length()) <
             0;
    }
    return a.length() < b.length();
  }

private:
  CXCompletionResult completionResult_;
  mutable int priority_;
  mutable std::string typedText_;
  mutable std::string briefDoc_;
};

CodeCompletion::CodeCompletion(TUManager &tuManager, bool detailedCompletions)
  : tuManager_(tuManager), detailedCompletions_(detailedCompletions) {
  TUManager::Settings settings;

  settings.parseTUOptions |= CXTranslationUnit_CacheCompletionResults;

#if defined(CINDEX_VERSION_MAJOR) && defined(CINDEX_VERSION_MINOR) &&          \
    (CINDEX_VERSION_MAJOR > 0 || CINDEX_VERSION_MINOR >= 6)
  settings.parseTUOptions |=
      CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;
#endif

  settingsID_ = tuManager.registerSettings(settings);
}

CodeCompletion::~CodeCompletion() {
  tuManager_.unregisterSettings(settingsID_);
}

std::string CodeCompletion::handleRequest(const JSONObjectWrapper &data,
                                          std::ostream &out) {
  bool valid = true;
  const std::string &file = data.check(L"file", valid);
  unsigned line = data.check(L"line", valid);
  unsigned column = data.check(L"column", valid);
  const std::vector<std::string> &flags = data.get(L"flags");

  out << ":results (";

  if (!valid) {
    std::clog << "Invalid completion request \"file\" and/or \"line\""
                 " and/or \"column\" are invalid." << std::endl;
  } else if (CXTranslationUnit tu = tuManager_.parse(file, flags)) {
    // TODO: enhance ? actually the function return false on error,
    //       we can display the error to the user maybe ?
    (void)complete(tu, file, line, column, out);
  }

  out << ")";
  return (detailedCompletions_ ? ":completion" : ":completion-simple");
}

void printSimpleResults(const CompletionResults &completions,
                        std::ostream &out) {
  for (CompletionResults::const_iterator it = completions.begin(),
                                         end = completions.end(),
                                         previous = completions.end();
       it != end;
       previous = it++) {
    const std::string &typedText(it->getTypedText());

    if (!typedText.empty()) {

      // XXX: Since results are sorted we avoid duplicates by looking at
      //      the previous value.
      if (previous == end || previous->getTypedText() != typedText) {
        out << typedText << " ";
      }
    }
  }
}

bool CodeCompletion::complete(CXTranslationUnit &tu,
                              const std::string &filename,
                              unsigned line,
                              unsigned column,
                              std::ostream &out) {
  if (CXCodeCompleteResults *completions =
          clang_codeCompleteAt(tu,
                               filename.c_str(),
                               line,
                               column,
                               0,
                               0,
#if defined(CINDEX_VERSION_MAJOR) && defined(CINDEX_VERSION_MINOR) &&          \
    (CINDEX_VERSION_MAJOR > 0 || CINDEX_VERSION_MINOR >= 6)
                               CXCodeComplete_IncludeBriefComments |
#endif
                                   // CXCodeComplete_IncludeCodePatterns |
                                   clang_defaultCodeCompleteOptions())) {
    handleDiagnostics(completions);
    CompletionResults candidates(completions->NumResults);

    for (unsigned i = 0; i != candidates.size(); ++i) {
      candidates[i].setCompletionResult(completions->Results[i]);
    }

    std::sort(candidates.begin(), candidates.end());

    if (detailedCompletions_) {
      printDetailedResults(candidates, out);
    } else {
      printSimpleResults(candidates, out);
    }

    clang_disposeCodeCompleteResults(completions);

    return true;
  }

  // FIXME: really an error or just no results ?
  return false;
}

void
CodeCompletion::handleDiagnostics(CXCodeCompleteResults *completions) const {
  if (unsigned numErrors = clang_codeCompleteGetNumDiagnostics(completions)) {
    std::clog << numErrors << " errors/warnings found during completion.\n";
    // TODO: do not log warnings, only errors?
    for (unsigned i = 0; i < numErrors; i++) {
      CXDiagnostic diagnostic = clang_codeCompleteGetDiagnostic(completions, i);
      CXString s = clang_formatDiagnostic(
          diagnostic, clang_defaultDiagnosticDisplayOptions());

      std::clog << clang_getCString(s) << std::endl;
      clang_disposeString(s);
      clang_disposeDiagnostic(diagnostic);
    }
  }
}

namespace {

inline void chunkContentCell(const char *key,
                             const CompletionChunk &chunk,
                             std::ostream &out) {
  out << " (" << key << " . " << chunk.escapedContent() << ")";
}

void formatCompletionChunks(const CompletionChunk &start,
                            std::ostream &out,
                            bool *hasOptional = 0) {
  out << "(";

  if (hasOptional) {
    *hasOptional = false;
  }

  for (CompletionChunk chunk(start); chunk.hasNext(); chunk.next()) {
    switch (chunk.kind()) {

    case CXCompletionChunk_TypedText:
      out << " " << chunk.escapedContent();
      break;

    case CXCompletionChunk_ResultType:       chunkContentCell("r",  chunk, out); break;
    case CXCompletionChunk_Placeholder:      chunkContentCell("ph", chunk, out); break;
    case CXCompletionChunk_Text:             chunkContentCell("t",  chunk, out); break;
    case CXCompletionChunk_Informative:      chunkContentCell("i",  chunk, out); break;
    case CXCompletionChunk_CurrentParameter: chunkContentCell("p",  chunk, out); break;

    case CXCompletionChunk_LeftParen:       out << " ?(";  break;
    case CXCompletionChunk_RightParen:      out << " ?)";  break;
    case CXCompletionChunk_LeftBracket:     out << " ?[";  break;
    case CXCompletionChunk_RightBracket:    out << " ?]";  break;
    case CXCompletionChunk_LeftBrace:       out << " ?{";  break;
    case CXCompletionChunk_RightBrace:      out << " ?}";  break;
    case CXCompletionChunk_LeftAngle:       out << " ?<";  break;
    case CXCompletionChunk_RightAngle:      out << " ?>";  break;
    case CXCompletionChunk_Comma:           out << " ?,";  break;
    case CXCompletionChunk_Colon:           out << " ?:";  break;
    case CXCompletionChunk_SemiColon:       out << " ?;";  break;
    case CXCompletionChunk_Equal:           out << " ?=";  break;
    case CXCompletionChunk_HorizontalSpace: out << " ? ";  break;
    case CXCompletionChunk_VerticalSpace:   out << " ?\n"; break;

    case CXCompletionChunk_Optional:
      if (hasOptional) {
        *hasOptional = true;
      }
      out << "(opt . ";
      formatCompletionChunks(chunk.optionalChunks(), out);
      out << ")";
      break;

    default:
      break;
    }
  }

  out << ")";
}

} // unnamed namespace

void CodeCompletion::printDetailedResults(const CompletionResults &completions,
                                          std::ostream &out) {
  for (CompletionResults::const_iterator it = completions.begin(),
                                         end = completions.end();
       it != end;
       ++it) {
    bool hasOptional = false;

    out << "\n(";
    formatCompletionChunks(it->firstChunk(), out, &hasOptional);
    if (hasOptional) {
      out << " (opt . t)";
    }

    const std::string &briefDoc = it->briefDoc();
    if (!briefDoc.empty())
      out << " (b . " << briefDoc << ")";
    out << " (p . " << it->priority() << "))";
  }
}
