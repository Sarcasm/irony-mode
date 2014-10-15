/*
  This program takes some forward its command line arguments to libclang and
  returns the number of diagnostics that occured during the parsing.

  It is used during CMake generation to adjust the default parameters to
  libclang.
*/

#include <clang-c/Index.h>

#include <stdio.h>

int main(int argc, const char *argv[]) {
  for (int i = 1; i < argc; ++i) {
    fprintf(stdout, "argv[%d]: %s\n", i, argv[i]);
  }

  CXIndex Idx = clang_createIndex(0, 0);
  CXTranslationUnit TU = clang_parseTranslationUnit(
      Idx, NULL, &argv[1], argc - 1, 0, 0, CXTranslationUnit_None);
  int NumDiagnostics;

  if (TU == NULL) {
    NumDiagnostics = 1;
    fprintf(stderr, "failed to create translation unit!\n");
  } else {
    int i;

    NumDiagnostics = clang_getNumDiagnostics(TU);

    for (i = 0; i < NumDiagnostics; ++i) {
      CXDiagnostic Diag = clang_getDiagnostic(TU, i);
      CXString DiagStr =
          clang_formatDiagnostic(Diag, clang_defaultDiagnosticDisplayOptions());

      fprintf(stderr, "%s\n", clang_getCString(DiagStr));

      clang_disposeString(DiagStr);
      clang_disposeDiagnostic(Diag);
    }

    clang_disposeTranslationUnit(TU);
  }

  clang_disposeIndex(Idx);
  return NumDiagnostics;
}
