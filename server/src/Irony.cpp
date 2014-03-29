#include "Irony.h"

#include <iostream>

Irony::Irony() {
}

void Irony::check(const std::string &file,
                  const std::vector<std::string> &flags) {
  std::cout << "(";

  unsigned numDiag = 0;
  int fatals = 0;
  int errors = 0;
  int warnings = 0;

  CXTranslationUnit tu = tuManager_.parse(file, flags);

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
