if (GENERATE_DOXYGEN)
  find_package (Doxygen REQUIRED)

  set (HAS_DOT_VALUE "NO")
  if (DOXYGEN_DOT_FOUND)
    set (HAS_DOT_VALUE "YES")
  endif()

  configure_file(irony-server.cfg.in irony-server.cfg @ONLY)

  add_custom_target(doxygen
    ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/irony-server.cfg
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating API documentation with Doxygen" VERBATIM)
endif()
