# On MS-Windows, emacs_dir is a special environment variable, which
# indicates the full path of the directory in which Emacs is
# installed.
#
# There is no standard location that I know of for Emacs on Windows so
# using this special environment variable will at least help people
# who build the server from inside Emacs.
if(DEFINED ENV{emacs_dir})
  list(APPEND EMACS_EXECUTABLE_HINTS $ENV{emacs_dir}/bin)
endif()

find_program(EMACS_EXECUTABLE emacs
  HINTS ${EMACS_EXECUTABLE_HINTS})

if (EMACS_EXECUTABLE)
  message(STATUS "Found emacs: ${EMACS_EXECUTABLE}")
else()
  message(WARNING "emacs not found: elisp tests will be skipped!")
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
ert_test(irony-cdb-json-el irony-cdb-json.el)
