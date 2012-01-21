/**
 * \file   SyntaxChecker.cpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Wed Aug 24 14:07:09 2011
 *
 * \brief  SyntaxChecker implementation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "plugins/SyntaxChecker.h"

#include <iostream>

#include "str/to_string.hpp"

SyntaxChecker::SyntaxChecker()
{ }

SyntaxChecker::~SyntaxChecker()
{ }

std::string SyntaxChecker::handleRequest(TUManager &               tuManager,
                                         const JSONObjectWrapper & data,
                                         std::string &             buf)
{
  bool                             valid = true;
  const std::string &              file  = data.check(L"file", valid);
  const std::vector<std::string> & flags = data.get(L"flags");

  buf += ":diagnostics (";

  if (! valid)
    {
      std::clog << "invalid/incomplete data for syntax checking." << std::endl;
    }
  else if (CXTranslationUnit tu = tuManager.parse(file, flags))
    {
      unsigned numDiagnostic   = clang_getNumDiagnostics(tu);
      unsigned firstDiagnostic = 0;

      // The assumption is that for every note there was a previous
      // warning/error/... that needed this note to be complete. So to
      // begin we skip each note without any previous warning/error.
      while (firstDiagnostic < numDiagnostic &&
             clang_getDiagnosticSeverity(clang_getDiagnostic(tu, firstDiagnostic)) == CXDiagnostic_Note)
        ++firstDiagnostic;

      for (unsigned i = firstDiagnostic; i < numDiagnostic; ++i)
        {
          CXDiagnostic         diagnostic = clang_getDiagnostic(tu, i);
          CXDiagnosticSeverity severity   = clang_getDiagnosticSeverity(diagnostic);

          // Close previous opened parenthesis
          if (severity != CXDiagnostic_Note && i != firstDiagnostic)
            buf += "))";

          buf += "\n(";
          formatDiagnostic(diagnostic, buf);

          if (severity == CXDiagnostic_Note)
            buf += ")";
          else
            buf += " :notes (";
        }

      // close the last diagnostic if any
      if (firstDiagnostic < numDiagnostic)
        buf += "))";
    }

  buf += ")";
  return ":syntax-checking";
}

void SyntaxChecker::formatDiagnostic(const CXDiagnostic & diagnostic,
                                     std::string &        buf)
{
  //
  // Severity
  //
  buf += ":severity ";
  switch (clang_getDiagnosticSeverity(diagnostic)) {
  case CXDiagnostic_Ignored: buf += ":ignored"; break;
  case CXDiagnostic_Note:    buf += ":note";    break;
  case CXDiagnostic_Warning: buf += ":warning"; break;
  case CXDiagnostic_Error:   buf += ":error";   break;
  case CXDiagnostic_Fatal:   buf += ":fatal";   break;
  }

  //
  // Location
  //
  buf += " :location ";
  formatSourceLocation(clang_getDiagnosticLocation(diagnostic), buf);

  //
  // Diagnostic text
  //
  CXString spelling = clang_getDiagnosticSpelling(diagnostic);

  buf += " :diagnostic ";
  buf.append("\"").append(clang_getCString(spelling)).append("\"");

  clang_disposeString(spelling);

  //
  // Compiler flags that produce or remove the diagnostic
  //
  // (flag-that-produce-the-diagnostic . flag-to-disable-the-diagnostic)
  // i.e: :flags ("-Wuninitialized" . "-Wno-uninitialized")
  //
  CXString disable;
  CXString option = clang_getDiagnosticOption(diagnostic, &disable);

  buf += " :flags ";
  buf.append("(\"").append(clang_getCString(option))
    .append("\" . \"").append(clang_getCString(disable)).append("\")");

  clang_disposeString(option);
  clang_disposeString(disable);

  //
  // Category
  //
  unsigned categoryNumber = clang_getDiagnosticCategory(diagnostic);
  CXString category       = clang_getDiagnosticCategoryName(categoryNumber);

  buf += " :category ";
  buf.append("\"").append(clang_getCString(category)).append("\"");

  clang_disposeString(category);

  //
  // Ranges
  //
  // format:
  // ((\"file\" offset (line . column)) ...
  //
  unsigned numRanges = clang_getDiagnosticNumRanges(diagnostic);

  buf += " :ranges (";
  for (unsigned i = 0; i < numRanges; ++i)
    formatSourceRange(clang_getDiagnosticRange(diagnostic, i), buf);
  buf += ")";

  //
  // Fix-it hints
  //
  buf += " :fix-its (";
  formatFitItHints(diagnostic, buf);
  buf += ")";
}

void SyntaxChecker::formatSourceLocation(const CXSourceLocation & location,
                                         std::string &            buf)
{
  if (clang_equalLocations(location, clang_getNullLocation()))
    {
      buf += " nil ";
      return ;
    }

  CXFile   file;
  unsigned line, column, offset;

  clang_getInstantiationLocation(location, &file, &line, &column, &offset);
  CXString filename = clang_getFileName(file);


  // ("filename" offset (line . column))
  buf.append("(\"").append(clang_getCString(filename)) // filename
    .append("\" ").append(str::to_string(offset))      // offset
    .append(" (").append(str::to_string(line))         // line
    .append(" . ").append(str::to_string(column)).append("))"); // column

  clang_disposeString(filename);
}

void SyntaxChecker::formatSourceRange(const CXSourceRange & range,
                                      std::string &         buf)
{
  buf += "(";
  formatSourceLocation(clang_getRangeStart(range), buf);
  buf += " . ";
  formatSourceLocation(clang_getRangeEnd(range), buf);
  buf += ")";
}

void SyntaxChecker::formatFitItHints(const CXDiagnostic & diagnostic,
                                     std::string &        buf)
{
  unsigned numFixIts = clang_getDiagnosticNumFixIts(diagnostic);

  for (unsigned i = 0; i < numFixIts; ++i)
    {
      CXSourceRange replacementRange;
      CXString      fixItHint = clang_getDiagnosticFixIt(diagnostic, i,
                                                         &replacementRange);

      buf.append("(\"").append(clang_getCString(fixItHint)).append("\" . ");
      formatSourceRange(replacementRange, buf);
      buf += ")";

      clang_disposeString(fixItHint);
    }
}
