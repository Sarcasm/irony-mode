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
    if (clang_getCompletionChunkKind(completionString, i) == CXCompletionChunk_TypedText)
      {
        ClangString text(clang_getCompletionChunkText(completionString,
                                                      i), ClangString::Escape);

        return text.asString();
      }
  }

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

void CodeCompletion::printDetailedResult(CXCodeCompleteResults *completions,
                                         std::ostream & out)
{
  for (unsigned i = 0; i != completions->NumResults; ++i)
    {
      CXCompletionResult result = completions->Results[i];

      out << "\n";
      out << "(";
      formatCompletionCursorKind(result.CursorKind, out);
      out << " . ";
      formatCompletionString(result.CompletionString, out);
      out << ")";
    }
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
      std::clog << numErrors << " errors found during completion." << std::endl;
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

namespace {

const char *tryTextKindIdentifier(CXCompletionChunkKind chunkKind)
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

const char *completionCursorKindIdentifier(CXCursorKind  cursorKind)
{
  static const struct cursorKindToString {
    CXCursorKind  kind;
    const char   *str;
  } table[] = {
    {CXCursor_UnexposedDecl,                       ":UnexposedDecl"},
    {CXCursor_StructDecl,                          ":StructDecl"},
    {CXCursor_UnionDecl,                           ":UnionDecl"},
    {CXCursor_ClassDecl,                           ":ClassDecl"},
    {CXCursor_EnumDecl,                            ":EnumDecl"},
    {CXCursor_FieldDecl,                           ":FieldDecl"},
    {CXCursor_EnumConstantDecl,                    ":EnumConstantDecl"},
    {CXCursor_FunctionDecl,                        ":FunctionDecl"},
    {CXCursor_VarDecl,                             ":VarDecl"},
    {CXCursor_ParmDecl,                            ":ParmDecl"},
    {CXCursor_ObjCInterfaceDecl,                   ":ObjCInterfaceDecl"},
    {CXCursor_ObjCCategoryDecl,                    ":ObjCCategoryDecl"},
    {CXCursor_ObjCProtocolDecl,                    ":ObjCProtocolDecl"},
    {CXCursor_ObjCPropertyDecl,                    ":ObjCPropertyDecl"},
    {CXCursor_ObjCIvarDecl,                        ":ObjCIvarDecl"},
    {CXCursor_ObjCInstanceMethodDecl,              ":ObjCInstanceMethodDecl"},
    {CXCursor_ObjCClassMethodDecl,                 ":ObjCClassMethodDecl"},
    {CXCursor_ObjCImplementationDecl,              ":ObjCImplementationDecl"},
    {CXCursor_ObjCCategoryImplDecl,                ":ObjCCategoryImplDecl"},
    {CXCursor_TypedefDecl,                         ":TypedefDecl"},
    {CXCursor_CXXMethod,                           ":CXXMethod"},
    {CXCursor_Namespace,                           ":Namespace"},
    {CXCursor_LinkageSpec,                         ":LinkageSpec"},
    {CXCursor_Constructor,                         ":Constructor"},
    {CXCursor_Destructor,                          ":Destructor"},
    {CXCursor_ConversionFunction,                  ":ConversionFunction"},
    {CXCursor_TemplateTypeParameter,               ":TemplateTypeParameter"},
    {CXCursor_NonTypeTemplateParameter,            ":NonTypeTemplateParameter"},
    {CXCursor_TemplateTemplateParameter,           ":TemplateTemplateParameter"},
    {CXCursor_FunctionTemplate,                    ":FunctionTemplate"},
    {CXCursor_ClassTemplate,                       ":ClassTemplate"},
    {CXCursor_ClassTemplatePartialSpecialization,  ":ClassTemplatePartialSpecialization"},
    {CXCursor_NamespaceAlias,                      ":NamespaceAlias"},
    {CXCursor_UsingDirective,                      ":UsingDirective"},
    {CXCursor_UsingDeclaration,                    ":UsingDeclaration"},
    {CXCursor_FirstDecl,                           ":FirstDecl"},
    {CXCursor_LastDecl,                            ":LastDecl"},
    {CXCursor_FirstRef,                            ":FirstRef"},
    {CXCursor_ObjCSuperClassRef,                   ":ObjCSuperClassRef"},
    {CXCursor_ObjCProtocolRef,                     ":ObjCProtocolRef"},
    {CXCursor_ObjCClassRef,                        ":ObjCClassRef"},
    {CXCursor_TypeRef,                             ":TypeRef"},
    {CXCursor_CXXBaseSpecifier,                    ":CXXBaseSpecifier"},
    {CXCursor_TemplateRef,                         ":TemplateRef"},
    {CXCursor_NamespaceRef,                        ":NamespaceRef"},
    {CXCursor_MemberRef,                           ":MemberRef"},
    {CXCursor_LabelRef,                            ":LabelRef"},
    {CXCursor_OverloadedDeclRef,                   ":OverloadedDeclRef"},
    {CXCursor_LastRef,                             ":LastRef"},
    {CXCursor_FirstInvalid,                        ":FirstInvalid"},
    {CXCursor_InvalidFile,                         ":InvalidFile"},
    {CXCursor_NoDeclFound,                         ":NoDeclFound"},
    {CXCursor_NotImplemented,                      ":NotImplemented"},
    {CXCursor_InvalidCode,                         ":InvalidCode"},
    {CXCursor_LastInvalid,                         ":LastInvalid"},
    {CXCursor_FirstExpr,                           ":FirstExpr"},
    {CXCursor_UnexposedExpr,                       ":UnexposedExpr"},
    {CXCursor_DeclRefExpr,                         ":DeclRefExpr"},
    {CXCursor_MemberRefExpr,                       ":MemberRefExpr"},
    {CXCursor_CallExpr,                            ":CallExpr"},
    {CXCursor_ObjCMessageExpr,                     ":ObjCMessageExpr"},
    {CXCursor_BlockExpr,                           ":BlockExpr"},
    {CXCursor_LastExpr,                            ":LastExpr"},
    {CXCursor_FirstStmt,                           ":FirstStmt"},
    {CXCursor_UnexposedStmt,                       ":UnexposedStmt"},
    {CXCursor_LabelStmt,                           ":LabelStmt"},
    {CXCursor_LastStmt,                            ":LastStmt"},
    {CXCursor_TranslationUnit,                     ":TranslationUnit"},
    {CXCursor_FirstAttr,                           ":FirstAttr"},
    {CXCursor_UnexposedAttr,                       ":UnexposedAttr"},
    {CXCursor_IBActionAttr,                        ":IBActionAttr"},
    {CXCursor_IBOutletAttr,                        ":IBOutletAttr"},
    {CXCursor_IBOutletCollectionAttr,              ":IBOutletCollectionAttr"},
    {CXCursor_LastAttr,                            ":LastAttr"},
    {CXCursor_PreprocessingDirective,              ":PreprocessingDirective"},
    {CXCursor_MacroDefinition,                     ":MacroDefinition"},
    {CXCursor_MacroInstantiation,                  ":MacroInstantiation"},
    {CXCursor_InclusionDirective,                  ":InclusionDirective"},
    {CXCursor_FirstPreprocessing,                  ":FirstPreprocessing"},
    {CXCursor_LastPreprocessing,                   ":LastPreprocessing"}
  };

  for (std::size_t i = 0; i < ARRAYSIZE_UNSAFE(table); ++i)
    if (table[i].kind == cursorKind)
      return table[i].str;
  return "nil";
}

} // anonymous namespace

void CodeCompletion::formatCompletionCursorKind(CXCursorKind   cursorKind,
                                                std::ostream & out)
{
  out << completionCursorKindIdentifier(cursorKind);
}

void CodeCompletion::appendConsCellResult(const std::string & keyword,
                                          const std::string & value,
                                          std::ostream &      out)
{
    out << "(" << keyword << " . " << value << ")";
}

void CodeCompletion::formatCompletionString(CXCompletionString & completionString,
                                            std::ostream &       out)
{
  bool hasOptional = false;

  out << "(:priority ";
  out << clang_getCompletionPriority(completionString);
  out << " :result ";

  if (detailedCompletions_)
    out << "(";

  for (unsigned i = 0, max = clang_getNumCompletionChunks(completionString);
       i < max;
       ++i)
    {
      CXCompletionChunkKind chunkKind = clang_getCompletionChunkKind(completionString, i);

      if (! detailedCompletions_)
        {
          if (chunkKind != CXCompletionChunk_TypedText)
            continue ;

          ClangString text(clang_getCompletionChunkText(completionString, i),
                           ClangString::Escape);

          if (! text.isNull()) {
            out << text.asString();
          }
          break ;               // skip the when found
        }

      if (const char *keyword = tryTextKindIdentifier(chunkKind))
        {
          ClangString text(clang_getCompletionChunkText(completionString, i),
                           ClangString::Escape);

          if (! text.isNull()) {
            appendConsCellResult(keyword, text.asString(), out);
          }
          continue ;            // kind was a 'chunk text'
        }

      if (tryFormattingKeywordSymbol(chunkKind, out))
        continue ;         // kind was convertible to a keyword symbol

      if (chunkKind == CXCompletionChunk_Optional) // optional, recursive call
        {
          CXCompletionString optionalString =
            clang_getCompletionChunkCompletionString(completionString, i);

          hasOptional = true;
          out << "(:optional . \n\t";
          formatCompletionString(optionalString, out);
          out << ")";
        }
    }

  if (detailedCompletions_)
    {
      out << ")";               // close ":result ("
      if (hasOptional)
        out << " :optional t";
    }

  out << ")";                   // close "(:priority"
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
                                                std::ostream &        out)
{
  for (std::size_t i = 0; i < arraysize(kindsToLispForm); ++i)
    if (kindsToLispForm[i].kind == kind)
      {
        appendConsCellResult(":symbol", kindsToLispForm[i].lispForm, out);
        return true;
      }
  return false;
}
