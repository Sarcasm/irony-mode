message (STATUS "looking for emacs executable")
find_program (EMACS_EXECUTABLE emacs)

if (EMACS_EXECUTABLE)
  message (STATUS "looking for emacs executable -- found")
  #
  # ert_test(name test_file)
  #
  # creates a test target named NAME which run the Elisp script
  # TEST_FILE using the Emacs ERT testing framework.
  #
  function(ert_test name test_file)
    add_test(${name}
      ${EMACS_EXECUTABLE} -batch
      -l ${CMAKE_CURRENT_SOURCE_DIR}/${test_file}
      -f ert-run-tests-batch-and-exit)
  endfunction()

else()
  message (STATUS "looking for emacs executable -- not found")
  message (WARNING "elisp tests will not run because the emacs executable is missing")

  function(ert_test name test_file)
    # empty definition
  endfunction()
endif()

ert_test(irony-el test-irony.el)
ert_test(irony-cdb-el test-irony-cdb.el)
