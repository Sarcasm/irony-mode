#include "Irony.h"

#include "ClangString.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <stdexcept>

Irony::Irony() {
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

std::ostream &operator<<(std::ostream &os, const Candidate &c) {
  return os << "(" << c.getTypedText() << ")";
}

typedef std::vector<Candidate> CompletionResults;

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

#if defined(CINDEX_VERSION_MAJOR) && defined(CINDEX_VERSION_MINOR) &&          \
    (CINDEX_VERSION_MAJOR > 0 || CINDEX_VERSION_MINOR >= 6)
  settings.parseTUOptions |=
      CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;
#endif

  (void)tuManager_.registerSettings(settings);

  std::cout << "(";

  CXTranslationUnit tu = tuManager_.parse(file, flags, unsavedFiles);

  if (!tu) {
    std::cout << ")\n";
    return;
  }

  if (CXCodeCompleteResults *completions =
          clang_codeCompleteAt(tu,
                               file.c_str(),
                               line,
                               col,
                               0,
                               0,
                               (clang_defaultCodeCompleteOptions() &
                                ~CXCodeComplete_IncludeCodePatterns)
#if defined(CINDEX_VERSION_MAJOR) && defined(CINDEX_VERSION_MINOR) &&          \
    (CINDEX_VERSION_MAJOR > 0 || CINDEX_VERSION_MINOR >= 6)
                                   |
                                   CXCodeComplete_IncludeBriefComments
#endif
                               )) {
    CompletionResults candidates(completions->NumResults);

    for (unsigned i = 0; i != candidates.size(); ++i) {
      candidates[i].setCompletionResult(completions->Results[i]);
    }

    std::sort(candidates.begin(), candidates.end());

    for (auto &candidate : candidates) {
      std::cout << candidate << "\n";
    }

    clang_disposeCodeCompleteResults(completions);
  }
  std::cout << ")\n";
}
