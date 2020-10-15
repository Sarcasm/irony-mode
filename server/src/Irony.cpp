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
#include <fstream>
#include <cctype>

namespace {

std::string cxStringToStd(CXString cxString) {
  std::string stdStr;

  if (const char *cstr = clang_getCString(cxString)) {
    stdStr = cstr;
  }

  clang_disposeString(cxString);
  return stdStr;
}

const char *diagnosticSeverity(const CXDiagnostic &diagnostic) {
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

void dumpDiagnostic(const CXDiagnostic &diagnostic) {
  std::string file;
  unsigned line = 0, column = 0, offset = 0;
  CXSourceLocation location = clang_getDiagnosticLocation(diagnostic);
  if (!clang_equalLocations(location, clang_getNullLocation())) {
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

  std::string message = cxStringToStd(clang_getDiagnosticSpelling(diagnostic));

  std::cout << '(' << support::quoted(file)    //
            << ' ' << line                     //
            << ' ' << column                   //
            << ' ' << offset                   //
            << ' ' << severity                 //
            << ' ' << support::quoted(message) //
            << ")\n";
}

bool readFileContent(const std::string &filename,
                     Irony::UnsavedBuffer &outBuf) {
  std::ifstream ifs(filename.c_str(),
                    std::ios::in | std::ios::binary | std::ios::ate);

  if (!ifs.is_open()) {
    return false;
  }

  // FIXME: it's possible that this method of reading the file is 100% reliable,
  // I can't confirm that tellg() is guaranteed to return a byte count.
  // std::streamoff does not mention 'byte'.
  // In practice it seems to work but this may be just luck.
  // See also this discussion:
  // - http://stackoverflow.com/questions/22984956/tellg-function-give-wrong-size-of-file/22986486#22986486
  auto nbytes = ifs.tellg();

  if (nbytes == std::ifstream::pos_type(-1)) {
    return false;
  }

  outBuf.resize(nbytes);
  ifs.seekg(0, std::ios::beg);

  ifs.read(&outBuf[0], outBuf.size());

  if (!ifs){
    outBuf.clear();
    return false;
  }

  return true;
}

} // unnamed namespace

Irony::Irony()
  : activeTu_(nullptr), activeCompletionResults_(nullptr), debug_(false) {
}

void Irony::parse(const std::string &file,
                  const std::vector<std::string> &flags) {
  resetCache();
  activeTu_ = tuManager_.parse(file, flags, cxUnsavedFiles_);
  file_ = file;

  if (activeTu_ == nullptr) {
    std::cout << "(error . ("
              << "parse-error"
              << " \"failed to parse file\""
              << " " << support::quoted(file) << "))\n";
    return;
  }

  std::cout << "(success . t)\n";
}

void Irony::diagnostics() const {
  unsigned diagnosticCount;

  if (activeTu_ == nullptr) {
    diagnosticCount = 0;
  } else {
    diagnosticCount = clang_getNumDiagnostics(activeTu_);
  }

  std::cout << "(\n";

  for (unsigned i = 0; i < diagnosticCount; ++i) {
    CXDiagnostic diagnostic = clang_getDiagnostic(activeTu_, i);

    dumpDiagnostic(diagnostic);

    clang_disposeDiagnostic(diagnostic);
  }

  std::cout << ")\n";
}

void Irony::resetCache() {
  activeTu_ = nullptr;

  if (activeCompletionResults_ != nullptr) {
    clang_disposeCodeCompleteResults(activeCompletionResults_);
    activeCompletionResults_ = nullptr;
  }
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

  // TODO: operator>> so that one can re-use string allocated buffer
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
                     const std::vector<std::string> &flags) {
  resetCache();

  if (CXTranslationUnit tu =
          tuManager_.getOrCreateTU(file, flags, cxUnsavedFiles_)) {
    activeCompletionResults_ =
        clang_codeCompleteAt(tu,
                             file.c_str(),
                             line,
                             col,
                             const_cast<CXUnsavedFile *>(cxUnsavedFiles_.data()),
                             cxUnsavedFiles_.size(),
                             (clang_defaultCodeCompleteOptions() &
                              ~CXCodeComplete_IncludeCodePatterns)
#if HAS_BRIEF_COMMENTS_IN_COMPLETION
                                 |
                                 CXCodeComplete_IncludeBriefComments
#endif
                             );
  }

  if (activeCompletionResults_ == nullptr) {
    std::cout << "(error . ("
              << "complete-error"
              << " \"failed to perform code completion\""
              << " " << support::quoted(file) << " " << line << " " << col
              << "))\n";
    return;
  }

  clang_sortCodeCompletionResults(activeCompletionResults_->Results,
                                  activeCompletionResults_->NumResults);

  std::cout << "(success . t)\n";
}

namespace {

bool hasUppercase(const std::string &prefix)
{
  for (char c : prefix) {
    if (std::isupper(c)) {
      return true;
    }
  }
  return false;
}

bool isEqual(const bool insensitive, const char a, const char b)
{
  if (insensitive) {
    return std::tolower(a) == std::tolower(b);
  } else {
    return a == b;
  }
}

bool startsWith(const std::string& str, const std::string &prefix, bool caseInsensitive)
{
  if (str.length() < prefix.length()) {
    return false;
  }

  const auto charCmp = [&](const char a, const char b) {
    return isEqual(caseInsensitive, a, b);
  };

  auto res = std::mismatch(prefix.begin(), prefix.end(), str.begin(), charCmp);
  return res.first == prefix.end();
}

bool isStyleCaseInsensitive(const std::string &prefix, PrefixMatchStyle style)
{
  if (style == PrefixMatchStyle::SmartCase) {
    // For SmartCase style, do case insensitive matching only there isn't upper
    // case letter.
    if (!hasUppercase(prefix)) {
      style = PrefixMatchStyle::CaseInsensitive;
    }
  }
  return style == PrefixMatchStyle::CaseInsensitive;
}

} // unnamed namespace

void Irony::completionDiagnostics() const {
  unsigned diagnosticCount;

  if (activeCompletionResults_ == nullptr) {
    diagnosticCount = 0;
  } else {
    diagnosticCount =
        clang_codeCompleteGetNumDiagnostics(activeCompletionResults_);
  }

  std::cout << "(\n";

  for (unsigned i = 0; i < diagnosticCount; ++i) {
    CXDiagnostic diagnostic =
        clang_codeCompleteGetDiagnostic(activeCompletionResults_, i);

    dumpDiagnostic(diagnostic);
    clang_disposeDiagnostic(diagnostic);
  }

  std::cout << ")\n";
}

void Irony::candidates(const std::string &prefix, PrefixMatchStyle style) const {
  if (activeCompletionResults_ == nullptr) {
    std::cout << "nil\n";
    return;
  }

  bool caseInsensitive = isStyleCaseInsensitive(prefix, style);

  CXCodeCompleteResults *completions = activeCompletionResults_;

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
    bool hasPrefix = true;


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
        if (!startsWith(typedtext, prefix, caseInsensitive)) {
          hasPrefix = false;
          break;
        }
        // annotation is what comes after the typedtext
        annotationStart = prototype.size();
        typedTextSet = true;
      }
    }

    if (!hasPrefix) {
      continue;
    }
    if (!typedTextSet) {
      // clang may generate candidates without any typedText, and we may
      // generate some output like:
      //    ("" 1 "bool" "" "hasUppercase(const std::string &prefix)"
      //     0 ("") available)
      // That will cause infinite completion in irony.el
      continue;
    }

#if HAS_BRIEF_COMMENTS_IN_COMPLETION
    brief = cxStringToStd(
        clang_getCompletionBriefComment(candidate.CompletionString));
#endif

    // see irony-completion.el#irony-completion-candidates
    std::cout << '(' << support::quoted(typedtext)
              << ' ' << priority
              << ' ' << support::quoted(resultType)
              << ' ' << support::quoted(brief)
              << ' ' << support::quoted(prototype)
              << ' ' << annotationStart
              << " (" << support::quoted(postCompCar);
    for (unsigned index : postCompCdr)
      std::cout << ' ' << index;
    std::cout << ")"
              << ")\n";
  }

  std::cout << ")\n";
}

void Irony::computeCxUnsaved() {
  cxUnsavedFiles_.clear();

  for (const auto &p : filenameToContent_) {
    CXUnsavedFile cxUnsavedFile;

    cxUnsavedFile.Filename = p.first.c_str();
    cxUnsavedFile.Contents = p.second.data();
    cxUnsavedFile.Length = p.second.size();
    cxUnsavedFiles_.push_back(cxUnsavedFile);
  }
}

void Irony::setUnsaved(const std::string &file,
                       const std::string &unsavedContentFile) {
  resetCache();

  UnsavedBuffer content;
  if (!readFileContent(unsavedContentFile, content)) {
    filenameToContent_.erase(file);
    std::cout << "(error . ("
              << "file-read-error"
              << " \"failed to read unsaved buffer\""
              << " " << support::quoted(file) << " "
              << support::quoted(unsavedContentFile) << ")\n";
  } else {
    filenameToContent_[file] = content;
    std::cout << "(success . t)\n";
  }

  computeCxUnsaved();
}

void Irony::resetUnsaved(const std::string &file) {
  resetCache();

  const auto erasedCount = filenameToContent_.erase(file);

  if (erasedCount == 0) {
    std::cout << "(error . ("
              << "no-such-entry"
              << " \"failed reset unsaved buffer\""
              << " " << support::quoted(file) << ")\n";
  } else {
    std::cout << "(success . t)\n";
  }

  computeCxUnsaved();
}

void Irony::getCompileOptions(const std::string &buildDir,
                              const std::string &file) const {
#if !(HAS_COMPILATION_DATABASE)

  (void)buildDir;
  (void)file;

  CXString cxVersionString = clang_getClangVersion();

  std::cout << "(error . ("
            << "unsupported"
            << " \"compilation database requires Clang >= 3.2\""
            << " " << support::quoted(clang_getCString(cxVersionString))
            << "))\n";

  clang_disposeString(cxVersionString);

  return;

#else
  CXCompilationDatabase_Error error;
  CXCompilationDatabase db =
      compDBCache_.fromDirectory(buildDir.c_str(), &error);

  switch (error) {
  case CXCompilationDatabase_CanNotLoadDatabase:
    std::cout << "(error . ("
              << "cannot-load-database"
              << " \"failed to load compilation database from directory\""
              << " " << support::quoted(buildDir) << "))\n";
    return;

  case CXCompilationDatabase_NoError:
    break;
  }

  CXCompileCommands compileCommands =
      clang_CompilationDatabase_getCompileCommands(db, file.c_str());

  std::cout << "(success . (\n";

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

  std::cout << "))\n";

  clang_CompileCommands_dispose(compileCommands);
#endif
}
