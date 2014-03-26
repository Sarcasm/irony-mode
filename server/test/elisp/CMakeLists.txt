find_program (EMACS_EXECUTABLE emacs)

if (EMACS_EXECUTABLE)
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
  message (WARNING "emacs executable not found, elisp tests will not be run")

  function(ert_test name test_file)
    # empty definition
  endfunction()
endif()

ert_test(irony-el test-irony.el)
ert_test(irony-cdb-el test-irony-cdb.el)
