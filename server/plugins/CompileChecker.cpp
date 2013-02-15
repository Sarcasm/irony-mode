/**
 * \file   CompileChecker.cpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Thu Feb 14 20:37:23 2013
 *
 * \brief  See CompileChecker.h.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "CompileChecker.h"

#include <iostream>

#include "str/to_string.hpp"

CompileChecker::CompileChecker(TUManager & tuManager)
  : tuManager_(tuManager)
{ }

CompileChecker::~CompileChecker()
{ }

std::string CompileChecker::handleRequest(const JSONObjectWrapper & data,
                                          std::string &             buf)
{
  bool                             valid = true;
  const std::string &              file  = data.check(L"file", valid);
  const std::vector<std::string> & flags = data.get(L"flags");

  if (! valid) {
    std::clog << "invalid/incomplete data for compile-check." << std::endl;
  }

  buf += ":stats ";

  if (CXTranslationUnit tu = tuManager_.parse(file, flags)) {
    unsigned numDiag  = clang_getNumDiagnostics(tu);
    unsigned fatals   = 0;
    unsigned errors   = 0;
    unsigned warnings = 0;

    for (unsigned i = 0; i < numDiag; ++i) {
      CXDiagnostic diagnostic = clang_getDiagnostic(tu, i);

      switch (clang_getDiagnosticSeverity(diagnostic)) {
      case CXDiagnostic_Fatal:   fatals++;   break ;
      case CXDiagnostic_Error:   errors++;   break ;
      case CXDiagnostic_Warning: warnings++; break ;
      default: /* shutdown warning */        break ;
      }

      clang_disposeDiagnostic(diagnostic);
    }

    if (fatals == 0 && errors == 0 && warnings == 0) {
      buf += "t";               // file ok
    } else {
      buf += "(";
      if (fatals != 0) {
        buf.append(":fatal-errors ").append(str::to_string<int>(fatals));
      }
      if (errors != 0) {
        buf.append(" :errors ").append(str::to_string<int>(errors));
      }
      if (warnings != 0) {
        buf.append(" :warnings ").append(str::to_string<int>(warnings));
      }
      buf += ")";
    }
  } else {
    buf += "nil";               // couldn't create a translation unit'
  }

  return ":compile-check";
}
