/**
 * \file   CodeCompletion.cpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Thu Jul 21 09:17:33 2011
 *
 * \brief  Completion plugin implementation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "CodeCompletion.h"

#include <cstddef>
#include <iostream>
#include <iterator>
#include <set>
#include <string>

#include "util/arraysize.hpp"

#include "ClangString.h"

namespace {

class ClangCompletionChunk
{
public:
  ClangCompletionChunk(CXCompletionString completionString,
                       unsigned           chunkNumber)
    : completionString_(completionString)
    , chunkNumber_(chunkNumber)
  { }

  CXCompletionChunkKind kind() const
  {
    return clang_getCompletionChunkKind(completionString_, chunkNumber_);
  }

  std::string escapedContent() const
  {
    return ClangString(text(), ClangString::Escape).asString();
  }

  CXCompletionString completionString() const
  {
    return clang_getCompletionChunkCompletionString(completionString_,
                                                    chunkNumber_);
  }

private:
  ClangCompletionChunk(const ClangCompletionChunk &);
  ClangCompletionChunk & operator=(const ClangCompletionChunk &);

  CXString text() const
  {
    return clang_getCompletionChunkText(completionString_, chunkNumber_);
  }

private:
  const CXCompletionString completionString_;
  const unsigned           chunkNumber_;
};


} // unnamed namespace

CodeCompletion::CodeCompletion(TUManager & tuManager,
                               bool        detailedCompletions)
  : tuManager_(tuManager)
  , detailedCompletions_(detailedCompletions)
{
  TUManager::Settings settings;

  settings.parseTUOptions |= CXTranslationUnit_CacheCompletionResults;

  settingsID_ = tuManager.registerSettings(settings);
}

CodeCompletion::~CodeCompletion()
{
  tuManager_.unregisterSettings(settingsID_);
}

std::string CodeCompletion::handleRequest(const JSONObjectWrapper & data,
                                          std::ostream &            out)
{
  bool                             valid  = true;
  const std::string &              file   = data.check(L"file", valid);
  unsigned                         line   = data.check(L"line", valid);
  unsigned                         column = data.check(L"column", valid);
  const std::vector<std::string> & flags  = data.get(L"flags");

  out << ":results (";

  if (! valid)
    {
      std::clog << "Invalid completion request \"file\" and/or \"line\""
        " and/or \"column\" are invalid." << std::endl;
    }
  else if (CXTranslationUnit tu = tuManager_.parse(file, flags))
    {
      // TODO: enhance ? actually the function return false on error,
      //       we can display the error to the user maybe ?
      (void) complete(tu, file, line, column, out);
    }

  out << ")";
  return (detailedCompletions_ ? ":completion" : ":completion-simple");
}

namespace
{
std::string findTypedTextChunk(const CXCompletionString & completionString)
{
  unsigned chunksSize = clang_getNumCompletionChunks(completionString);

  for (unsigned i = 0; i < chunksSize; ++i) {
    ClangCompletionChunk chunk(completionString, i);

    if (chunk.kind() == CXCompletionChunk_TypedText) {
      ClangString text(clang_getCompletionChunkText(completionString,
                                                    i), ClangString::Escape);

      return text.asString();
    }
  }

  // Note: it makes sense that no typed is found, for example in
  // presence of 'CXCompletionChunk_CurrentParameter'.
  return "";
}
}

void printSimpleResult(CXCodeCompleteResults *completions, std::ostream & out)
{
  std::set<std::string> candidates;

  for (unsigned i = 0; i != completions->NumResults; ++i)
    {
      CXCompletionResult result = completions->Results[i];

      *candidates.insert(findTypedTextChunk(result.CompletionString)).first;
    }
  candidates.erase("");

  std::ostream_iterator<std::string> outIt(out, " ");
  std::copy(candidates.begin(), candidates.end(), outIt);
}

bool CodeCompletion::complete(CXTranslationUnit & tu,
                              const std::string & filename,
                              unsigned            line,
                              unsigned            column,
                              std::ostream &      out)
{
  if (CXCodeCompleteResults *completions =
      clang_codeCompleteAt(tu,
                           filename.c_str(),
                           line,
                           column,
                           0, 0,
                           clang_defaultCodeCompleteOptions() |
                           CXCodeComplete_IncludeCodePatterns))
    {
      handleDiagnostics(completions);

      if (detailedCompletions_) {
        printDetailedResult(completions, out);
      } else {
        printSimpleResult(completions, out);
      }

      clang_disposeCodeCompleteResults(completions);

      return true;
    }

  // FIXME: really an error or just no results ?
  return false;
}

void CodeCompletion::handleDiagnostics(CXCodeCompleteResults *completions) const
{
  if (unsigned numErrors = clang_codeCompleteGetNumDiagnostics(completions))
    {
      std::clog << numErrors << " errors/warnings found during completion.\n";
      // TODO: do not log warnings, only errors?
      for (unsigned i = 0; i < numErrors; i++) {
        CXDiagnostic diagnostic = clang_codeCompleteGetDiagnostic(completions, i);
        CXString s = clang_formatDiagnostic(diagnostic,
                                            clang_defaultDiagnosticDisplayOptions());

        std::clog << clang_getCString(s) << std::endl;
        clang_disposeString(s);
        clang_disposeDiagnostic(diagnostic);
      }
    }
}

void CodeCompletion::printDetailedResult(CXCodeCompleteResults *completions,
                                         std::ostream &         out)
{
  for (unsigned i = 0; i != completions->NumResults; ++i)
    {
      CXCompletionResult result = completions->Results[i];
      bool hasOptional = false;

      out << "\n(";
      formatCompletionString(result.CompletionString, out, &hasOptional);
      if (hasOptional) {
        out << " (opt . t)";
      }
      out << " (p . " << clang_getCompletionPriority(result.CompletionString)
          << "))";
    }
}

namespace {

inline void chunkContentCell(const char                   *key,
                             const ClangCompletionChunk &  chunk,
                             std::ostream &                out)
{
  out << " (" << key << " . " << chunk.escapedContent() << ")";
}

} // unnamed namespace

void CodeCompletion::formatCompletionString(CXCompletionString completionString,
                                            std::ostream &     out,
                                            bool              *hasOptional)
{
  out << "(";

  if (hasOptional) {
    *hasOptional = false;
  }

  for (unsigned i = 0, max = clang_getNumCompletionChunks(completionString); i < max; ++i) {
    ClangCompletionChunk chunk(completionString, i);

    switch (chunk.kind()) {

    case CXCompletionChunk_TypedText:
      out << " " << chunk.escapedContent(); break ;

    case CXCompletionChunk_ResultType:       chunkContentCell("r",  chunk, out); break ;
    case CXCompletionChunk_Placeholder:      chunkContentCell("ph", chunk, out); break ;
    case CXCompletionChunk_Text:             chunkContentCell("t",  chunk, out); break ;
    case CXCompletionChunk_Informative:      chunkContentCell("i",  chunk, out); break ;
    case CXCompletionChunk_CurrentParameter: chunkContentCell("p",  chunk, out); break ;

    case CXCompletionChunk_LeftParen:       out << " ?(";  break ;
    case CXCompletionChunk_RightParen:      out << " ?)";  break ;
    case CXCompletionChunk_LeftBracket:     out << " ?[";  break ;
    case CXCompletionChunk_RightBracket:    out << " ?]";  break ;
    case CXCompletionChunk_LeftBrace:       out << " ?{";  break ;
    case CXCompletionChunk_RightBrace:      out << " ?}";  break ;
    case CXCompletionChunk_LeftAngle:       out << " ?<";  break ;
    case CXCompletionChunk_RightAngle:      out << " ?>";  break ;
    case CXCompletionChunk_Comma:           out << " ?,";  break ;
    case CXCompletionChunk_Colon:           out << " ?:";  break ;
    case CXCompletionChunk_SemiColon:       out << " ?;";  break ;
    case CXCompletionChunk_Equal:           out << " ?=";  break ;
    case CXCompletionChunk_HorizontalSpace: out << " ? ";  break ;
    case CXCompletionChunk_VerticalSpace:   out << " ?\n"; break ;

    case CXCompletionChunk_Optional:
      if (hasOptional) {
        *hasOptional = true;
      }
      out << "(opt . ";
      formatCompletionString(chunk.completionString(), out);
      out << ")";
      break ;

    default:
      break ;
    }
  }

  out << ")";
}
