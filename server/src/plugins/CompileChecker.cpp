/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief See CompileChecker.h.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "CompileChecker.h"

#include <iostream>

CompileChecker::CompileChecker(TUManager &tuManager) : tuManager_(tuManager) {
}

CompileChecker::~CompileChecker() {
}

std::string CompileChecker::handleRequest(const JSONObjectWrapper &data,
                                          std::ostream &out) {
  bool valid = true;
  const std::string &file = data.check(L"file", valid);
  const std::vector<std::string> &flags = data.get(L"flags");

  if (!valid) {
    std::clog << "invalid/incomplete data for compile-check." << std::endl;
  }

  out << ":stats ";

  if (CXTranslationUnit tu = tuManager_.parse(file, flags)) {
    unsigned numDiag = clang_getNumDiagnostics(tu);
    unsigned fatals = 0;
    unsigned errors = 0;
    unsigned warnings = 0;

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
      default: /* shutdown warning */
        break;
      }

      clang_disposeDiagnostic(diagnostic);
    }

    if (fatals == 0 && errors == 0 && warnings == 0) {
      out << "t"; // file ok
    } else {
      out << "(";
      if (fatals != 0) {
        out << ":fatal-errors " << fatals;
      }
      if (errors != 0) {
        out << " :errors " << errors;
      }
      if (warnings != 0) {
        out << " :warnings " << warnings;
      }
      out << ")";
    }
  } else {
    out << "nil"; // couldn't create a translation unit'
  }

  return ":compile-check";
}
