/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief SyntaxChecker implementation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "plugins/SyntaxChecker.h"

#include "ClangString.h"

#include <iostream>
#include <cassert>

SyntaxChecker::SyntaxChecker(TUManager &tuManager) : tuManager_(tuManager) {
}

SyntaxChecker::~SyntaxChecker() {
}

std::string SyntaxChecker::handleRequest(const JSONObjectWrapper &data,
                                         std::ostream &out) {
  bool valid = true;
  const std::string &file = data.check(L"file", valid);
  const std::vector<std::string> &flags = data.get(L"flags");

  out << ":diagnostics (";

  if (!valid) {
    std::clog << "invalid/incomplete data for syntax checking." << std::endl;
  } else if (CXTranslationUnit tu = tuManager_.parse(file, flags)) {
    unsigned numDiagnostic = clang_getNumDiagnostics(tu);
    unsigned firstDiagnostic = 0;

    // The assumption is that for every note there was a previous
    // warning/error/... that needed this note to be complete. So to
    // begin we skip each note without any previous warning/error.
    while (firstDiagnostic < numDiagnostic &&
           clang_getDiagnosticSeverity(
               clang_getDiagnostic(tu, firstDiagnostic)) == CXDiagnostic_Note)
      ++firstDiagnostic;

    for (unsigned i = firstDiagnostic; i < numDiagnostic; ++i) {
      CXDiagnostic diagnostic = clang_getDiagnostic(tu, i);
      CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diagnostic);

      // Close previous opened parenthesis
      if (severity != CXDiagnostic_Note && i != firstDiagnostic)
        out << "))";

      out << "\n(";
      formatDiagnostic(diagnostic, out);

      if (severity == CXDiagnostic_Note)
        out << ")";
      else
        out << " :notes (";
    }

    // close the last diagnostic if any
    if (firstDiagnostic < numDiagnostic)
      out << "))";
  }

  out << ")";
  return ":syntax-checking";
}

void SyntaxChecker::formatDiagnostic(const CXDiagnostic &diagnostic,
                                     std::ostream &out) {
  //
  // Severity
  //
  out << ":severity ";
  switch (clang_getDiagnosticSeverity(diagnostic)) {
  case CXDiagnostic_Ignored:
    out << ":ignored";
    break;
  case CXDiagnostic_Note:
    out << ":note";
    break;
  case CXDiagnostic_Warning:
    out << ":warning";
    break;
  case CXDiagnostic_Error:
    out << ":error";
    break;
  case CXDiagnostic_Fatal:
    out << ":fatal";
    break;
  }

  //
  // Location
  //
  out << " :location ";
  formatSourceLocation(clang_getDiagnosticLocation(diagnostic), out);

  //
  // Diagnostic text
  //
  ClangString diagnosticStr(clang_getDiagnosticSpelling(diagnostic),
                            ClangString::AddQuotes);
  out << " :diagnostic ";
  out << diagnosticStr.asString();

  //
  // Compiler flags that produce or remove the diagnostic
  //
  // (flag-that-produce-the-diagnostic . flag-to-disable-the-diagnostic)
  // i.e: :flags ("-Wuninitialized" . "-Wno-uninitialized")
  //
  CXString disable;
  CXString option = clang_getDiagnosticOption(diagnostic, &disable);

  out << " :flags ";
  out << "(\"" << clang_getCString(option) << "\" . \""
      << clang_getCString(disable) << "\")";

  clang_disposeString(option);
  clang_disposeString(disable);

  //
  // Ranges
  //
  // format:
  // ((\"file\" offset (line . column)) ...
  //
  unsigned numRanges = clang_getDiagnosticNumRanges(diagnostic);

  out << " :ranges (";
  for (unsigned i = 0; i < numRanges; ++i) {
    const CXSourceRange &range = clang_getDiagnosticRange(diagnostic, i);

    assert(!clang_Range_isNull(range));
    formatSourceRange(range, out);
  }
  out << ")";

  //
  // Fix-it hints
  //
  out << " :fix-its (";
  formatFitItHints(diagnostic, out);
  out << ")";
}

void SyntaxChecker::formatSourceLocation(const CXSourceLocation &location,
                                         std::ostream &out) {
  if (clang_equalLocations(location, clang_getNullLocation())) {
    out << " nil ";
    return;
  }

  CXFile file;
  unsigned line, column, offset;

// clang_getInstantiationLocation() has been marked deprecated and
// is aimed to be replaced by clang_getExpansionLocation().
#if defined(CINDEX_VERSION_MAJOR) && defined(CINDEX_VERSION_MINOR) &&          \
    (CINDEX_VERSION_MAJOR > 0 || CINDEX_VERSION_MINOR >= 6)
  clang_getExpansionLocation(location, &file, &line, &column, &offset);
#else
  clang_getInstantiationLocation(location, &file, &line, &column, &offset);
#endif

  CXString filename = clang_getFileName(file);

  if (const char *filenameStr = clang_getCString(filename)) {
    // ("filename" offset (line . column))
    out << "(\"" << filenameStr // filename
        << "\" " << offset      // offset
        << " (" << line         // line
        << " . " << column      // column
        << "))";
  } else {
    // XXX: Absence of a filename is not interesting for the client
    // and is considered equivalent to a null location.
    out << " nil ";
  }

  clang_disposeString(filename);
}

void SyntaxChecker::formatSourceRange(const CXSourceRange &range,
                                      std::ostream &out) {
  out << "(";
  formatSourceLocation(clang_getRangeStart(range), out);
  out << " . ";
  formatSourceLocation(clang_getRangeEnd(range), out);
  out << ")";
}

void SyntaxChecker::formatFitItHints(const CXDiagnostic &diagnostic,
                                     std::ostream &out) {
  unsigned numFixIts = clang_getDiagnosticNumFixIts(diagnostic);

  for (unsigned i = 0; i < numFixIts; ++i) {
    CXSourceRange replacementRange;
    CXString fixItHint =
        clang_getDiagnosticFixIt(diagnostic, i, &replacementRange);

    out << "(\"" << clang_getCString(fixItHint) << "\" . ";
    formatSourceRange(replacementRange, out);
    out << ")";

    clang_disposeString(fixItHint);
  }
}
