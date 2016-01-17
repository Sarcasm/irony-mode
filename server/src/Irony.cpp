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

#include <boost/filesystem.hpp>

#include <algorithm>
#include <cassert>
#include <iostream>

using boost::filesystem::path;

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

#if HAS_COMPILATION_DATABASE

class CompilationDatabase {
public:
  explicit CompilationDatabase(std::string buildDir) : buildDir(buildDir) {
    db = clang_CompilationDatabase_fromDirectory(buildDir.c_str(), &error);

    switch (error) {
    case CXCompilationDatabase_CanNotLoadDatabase:
      std::clog << "I: could not load compilation database in '" << buildDir
                << "'\n";
      break;
    case CXCompilationDatabase_NoError:
      constructFileNames();
      break;
    }
  }

  CompilationDatabase &operator=(const CompilationDatabase&) = delete;
  CompilationDatabase(const CompilationDatabase&) = delete;
  CompilationDatabase &operator=(CompilationDatabase&&) = delete;
  CompilationDatabase(CompilationDatabase&&) = delete;

  ~CompilationDatabase() {
    if (error == CXCompilationDatabase_NoError)
      clang_CompilationDatabase_dispose(db);
  }

  bool isValid() const { return error == CXCompilationDatabase_NoError; }
  const std::string &getBuildDir() const { return buildDir; }
  const std::vector<std::string> &getAllFiles() const { return files; }
  std::vector<std::vector<std::string>>
  getCompileCommands(const std::string &file);
  std::vector<std::string> guessCompileCommand(const std::string &file);

private:
  void constructFileNames();

  std::string buildDir;
  CXCompilationDatabase db;
  CXCompilationDatabase_Error error;
  std::vector<std::string> files;
};

void CompilationDatabase::constructFileNames() {
  assert(error == CXCompilationDatabase_NoError &&
         "Should only be called if database was loaded!");

  CXCompileCommands cmds = clang_CompilationDatabase_getAllCompileCommands(db);
  unsigned ncmds = clang_CompileCommands_getSize(cmds);

  for (unsigned i = 0; i < ncmds; ++i) {
    CXCompileCommand cmd = clang_CompileCommands_getCommand(cmds, i);

    CXString wd = clang_CompileCommand_getDirectory(cmd);
    std::string workingDir(clang_getCString(wd));
    clang_disposeString(wd);

    std::string file;

    // Iterate through compile args to find file
    unsigned nargs = clang_CompileCommand_getNumArgs(cmd);
    for (unsigned j = 1; j < nargs; ++j) { // skip compiler
      CXString arg = clang_CompileCommand_getArg(cmd, j);
      std::string CompileArg(clang_getCString(arg));
      clang_disposeString(arg);

      if (CompileArg == "-o") {
        // Skip output file
        ++j;
        continue;
      }

      if (CompileArg[0] == '-')
        continue;

      // Found compiled file
      file = CompileArg;
      break;
    }

    path filePath(file);

    // Make absolute
    if (filePath.is_relative())
      filePath = path(workingDir) /= filePath;

    files.push_back(filePath.make_preferred().native());
  }

  clang_CompileCommands_dispose(cmds);
}

std::vector<std::vector<std::string>>
CompilationDatabase::getCompileCommands(const std::string &file) {
  std::vector<std::vector<std::string>> compileCommands;

  assert(isValid() && "Should have loaded the database without error!");

  CXCompileCommands cxCompileCommands =
      clang_CompilationDatabase_getCompileCommands(db, file.c_str());
  unsigned numCompileCommands =
      clang_CompileCommands_getSize(cxCompileCommands);

  compileCommands.resize(numCompileCommands);

  for (unsigned i = 0; i < numCompileCommands; ++i) {
    std::vector<std::string> &compileCommand = compileCommands[i];

    CXCompileCommand cxCompileCommand =
        clang_CompileCommands_getCommand(cxCompileCommands, i);
    unsigned numArgs = clang_CompileCommand_getNumArgs(cxCompileCommand);

    CXString directory = clang_CompileCommand_getDirectory(cxCompileCommand);
    compileCommand.reserve(numArgs + 1);
    compileCommand.push_back(clang_getCString(directory));
    clang_disposeString(directory);

    for (unsigned j = 0; j < numArgs; ++j) {
      CXString arg = clang_CompileCommand_getArg(cxCompileCommand, j);
      compileCommand.push_back(clang_getCString(arg));
      clang_disposeString(arg);
    }
  }

  clang_CompileCommands_dispose(cxCompileCommands);
  return compileCommands;
}

std::vector<std::string>
CompilationDatabase::guessCompileCommand(const std::string &file) {
  std::vector<std::string> compileCmd;
  // TODO: Implement
  return compileCmd;
}

bool Irony::loadCompilationDatabase(const std::string &buildDir) {
  if (db && db->getBuildDir() == buildDir)
    return true;

  db.reset(new CompilationDatabase(buildDir));

  if (!db->isValid()) {
    db.reset();
    return false;
  }
  return true;
}

#endif // HAS_COMPILATION_DATABASE

// The destructor of Irony needs to see the complete type CompilationDatabase
// because of the unique_ptr.
Irony::~Irony() = default;

void Irony::getAllFiles(const std::string &buildDir) {
#if HAS_COMPILATION_DATABASE
  if (!loadCompilationDatabase(buildDir)) {
    std::cout << "nil\n";
    return;
  }

  std::vector<std::string> files = db->getAllFiles();

  std::cout << "(\n";
  for (const std::string &file : files) {
    std::cout << support::quoted(file) << "\n";
  }
  std::cout << ")\n";

#else // !HAS_COMPILATION_DATABASE

  std::cout << "nil\n";

#endif
}

void Irony::getCompileOptions(const std::string &buildDir,
                              const std::string &file) {
#if !(HAS_COMPILATION_DATABASE)

  (void)buildDir;
  (void)file;

  std::cout << "nil\n";
  return;

#else

  if (!loadCompilationDatabase(buildDir)) {
    std::cout << "nil\n";
    return;
  }

  std::vector<std::vector<std::string>> compileCommands =
      db->getCompileCommands(file);

  std::cout << "(\n";
  for (const std::vector<std::string>& compileCommand : compileCommands) {
    assert(!compileCommand.empty() &&
           "Should at least contain the working directory!");

    std::cout << "( ";

    std::cout << "(";
    for (std::vector<std::string>::const_iterator
             i = std::next(compileCommand.begin()),
             e = compileCommand.end();
         i != e; ++i) {
      const std::string& compileArg = *i;
      std::cout << support::quoted(compileArg) << " ";
    }
    std::cout << ")";

    std::cout << " . ";

    std::cout << support::quoted(compileCommand[0]);
    std::cout << " )\n";

  }
  std::cout << ")\n";

#endif
}
