/**
 * \file   CodeCompletion.cpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Thu Jul 21 09:17:33 2011
 *
 * \brief  Completion plugin implementation.
 *
 *
 */

#include "CodeCompletion.hh"

#include <iostream>
#include <cstddef>
#include <string>

#include "to_string.hpp"
#include "utility.hpp"

CodeCompletion::CodeCompletion()
{ }

CodeCompletion::~CodeCompletion()
{ }

std::string CodeCompletion::handleRequest(TUManager &               tuManager,
                                          const JSONObjectWrapper & data,
                                          std::string &             buf)
{
  bool                             valid  = true;
  const std::string &              file   = data.check(L"file", valid);
  unsigned                         line   = data.check(L"line", valid);
  unsigned                         column = data.check(L"column", valid);
  const std::vector<std::string> & flags  = data.get(L"flags");

  buf += ":results (";

  if (not valid)
    {
      std::clog << "Invalid completion request \"file\" and/or \"line\""
        " and/or \"column\" are invalid." << std::endl;
    }
  else if (CXTranslationUnit tu = tuManager.parse(file, flags))
    {
      // TODO: enhance ? actually the function return false on error,
      // we can display the error to the user maybe ?
      complete(tu, file, line, column, buf);
    }

  buf += ")";
  return ":completion";
}

bool CodeCompletion::complete(CXTranslationUnit & tu,
                              const std::string & filename,
                              unsigned            line,
                              unsigned            column,
                              std::string &       buf)
{
  CXCodeCompleteResults   *completionResults
    = clang_codeCompleteAt(tu,
                           filename.c_str(),
                           line,
                           column,
                           0, 0,
                           clang_defaultCodeCompleteOptions() |
                           CXCodeComplete_IncludeCodePatterns);

  if (! completionResults)
    {
      // FIXME: really an error of just no results ?
      return false;
    }

  // FIXME: DEBUG
  if (unsigned numErrors = clang_codeCompleteGetNumDiagnostics(completionResults))
    {
      std::clog << numErrors << " erreurs trouvées." << std::endl;
      for (unsigned i = 0; i < numErrors; i++)
        {
          const CXDiagnostic& diag = clang_codeCompleteGetDiagnostic(completionResults, i);
          const CXString&     s    = clang_getDiagnosticSpelling(diag);

          std::clog << clang_getCString(s) << std::endl;
        }
    }
  // END DEBUG

  for (unsigned i = 0; i != completionResults->NumResults; ++i)
    {
      CXCompletionResult result = completionResults->Results[i];

      buf += "\n";
      formatCompletionString(result.CompletionString, buf);
    }

  clang_disposeCodeCompleteResults(completionResults);

  return true;
}

void CodeCompletion::appendConsCellResult(const std::string & keyword,
                                          const std::string & value,
                                          std::string &       buf,
                                          bool                needQuote)
{
  if (needQuote)
    buf.append("(").append(keyword).append(" . \"").append(value).append("\")");
  else
    buf.append("(").append(keyword).append(" . ").append(value).append(")");
}

namespace {
static const char *tryTextKindIdentifier(CXCompletionChunkKind chunkKind)
{
  switch (chunkKind)
    {
    case CXCompletionChunk_TypedText:   return ":typed-text";
    case CXCompletionChunk_Text:        return ":text";
    case CXCompletionChunk_Informative: return ":informative";
    case CXCompletionChunk_ResultType:  return ":result-type";
    case CXCompletionChunk_Placeholder: return ":place-holder";
    default:                            return 0;
    }
}
} // anonymous namespace

void CodeCompletion::formatCompletionString(CXCompletionString & completionString,
                                            std::string &        buf)
{
  bool hasOptional = false;

  buf += "(:priority ";
  buf += to_string<unsigned>(clang_getCompletionPriority(completionString));
  buf += " :result (";

  for (unsigned i = 0, max = clang_getNumCompletionChunks(completionString);
       i < max;
       ++i)
    {
      CXCompletionChunkKind chunkKind = clang_getCompletionChunkKind(completionString, i);

      if (tryFormattingKeywordSymbol(chunkKind, buf))
        continue ;         // kind was convertible to a keyword symbol

      if (const char *keyword = tryTextKindIdentifier(chunkKind))
        {
          CXString text = clang_getCompletionChunkText(completionString, i);

          if (const char *ctext = clang_getCString(text)) {
            appendConsCellResult(keyword, ctext, buf, true);
          }
          clang_disposeString(text);
          continue ;            // kind was a 'chunk text'
        }

      if (chunkKind == CXCompletionChunk_Optional) // optional, recursive call
        {
          CXCompletionString optionalString =
            clang_getCompletionChunkCompletionString(completionString, i);

          hasOptional = true;
          buf += "(:optional . \n\t";
          formatCompletionString(optionalString, buf);
          buf += ")";
        }
    }

  buf.append(")")               // close ":result ("
    .append((hasOptional ? " :optional t)" : ")")); // close "(:priority"
}

namespace {
static const struct kindsToLispForm_t // arraysize() can't work with
// anonymous structures.
{
  CXCompletionChunkKind kind;
  const std::string     lispForm;
} kindsToLispForm[] =
  {
    {CXCompletionChunk_LeftParen,           ":left-paren"},         // '('
    {CXCompletionChunk_RightParen,          ":right-paren"},        // ')'
    {CXCompletionChunk_LeftBracket,         ":left-bracket"},       // '['
    {CXCompletionChunk_RightBracket,        ":right-bracket"},      // ']'
    {CXCompletionChunk_LeftBrace,           ":left-brace"},         // '{'
    {CXCompletionChunk_RightBrace,          ":right-brace"},        // '}'
    {CXCompletionChunk_LeftAngle,           ":left-angle"},         // '<'
    {CXCompletionChunk_RightAngle,          ":right-angle"},        // '>'
    {CXCompletionChunk_Comma,               ":comma"},              // ','
    {CXCompletionChunk_Colon,               ":colon"},              // ':'
    {CXCompletionChunk_SemiColon,           ":semi-colon"},         // ';'
    {CXCompletionChunk_Equal,               ":equal"},              // '='
    {CXCompletionChunk_HorizontalSpace,     ":horizontal-space"},   // ' '
    {CXCompletionChunk_VerticalSpace,       ":vertical-space"}      // '\n'
  };
}

bool CodeCompletion::tryFormattingKeywordSymbol(CXCompletionChunkKind kind,
                                                std::string &         buf)
{
  for (std::size_t i = 0; i < arraysize(kindsToLispForm); ++i)
    if (kindsToLispForm[i].kind == kind)
      {
        appendConsCellResult(":symbol", kindsToLispForm[i].lispForm, buf);
        return true;
      }
  return false;
}
