find_program(EMACS_EXECUTABLE emacs)

if (NOT EMACS_EXECUTABLE)
  message(WARNING "emacs executable not found, elisp tests will not be run")
  return()
endif()

#
# ert_test(name test_file)
#
# creates a test target named NAME which run the Elisp script TEST_FILE using
# the Emacs ERT testing framework.
#
# FIXME: assumes MELPA is configured...
function(ert_test name test_file)
  add_test(${name}
    ${EMACS_EXECUTABLE} -batch
    -l package
    --eval "(package-initialize) (unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))"
    -l ${CMAKE_CURRENT_SOURCE_DIR}/${test_file}
    -f ert-run-tests-batch-and-exit)
endfunction()

ert_test(irony-el irony.el)
ert_test(irony-cdb-el irony-cdb.el)
