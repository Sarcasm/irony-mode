add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure)

add_subdirectory (elisp)
