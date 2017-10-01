#
# Get the Clang resource directory.
#
# If found the following variable will be set:
# - CLANG_RESOURCE_DIR
#
set(CHECK_CLANG_RESOURCE_DIR_CHECKER_CODE_IN
  ${CMAKE_CURRENT_LIST_DIR}/LibClangDiagnosticsChecker.cpp)

function(check_clang_resource_dir)
  if (CLANG_RESOURCE_DIR)
    return()                    # already in cache
  endif()

  message(STATUS "Detecting Clang resource directory")
  find_package (LibClang REQUIRED)
  
  set(checker_code
    ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/LibClangDiagnosticsChecker.cpp)
  set(checked_file
    "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check-libclang-stddef.cpp")

  configure_file(${CHECK_CLANG_RESOURCE_DIR_CHECKER_CODE_IN}
    ${checker_code} COPYONLY)
  file(WRITE "${checked_file}" "#include <stddef.h>\n")

  # Paths stolen from Rip-Rip/clang_complete#getBuiltinHeaderPath()
  find_path(CHECK_CLANG_RESOURCE_DIR include/stddef.h
    NO_DEFAULT_PATH
    # the default path, favor this one over the other, in case a specific
    # libclang has been chosen.
    HINTS "${LIBCLANG_LIBRARY_DIR}/../lib/clang"
    # other, distribution specific, paths
    PATHS
    "${LIBCLANG_LIBRARY_DIR}/../clang" # Gentoo
    "${LIBCLANG_LIBRARY_DIR}/clang"    # openSUSE, Windows
    "${LIBCLANG_LIBRARY_DIR}/"         # Google
    "/usr/lib64/clang"                 # x86_64 (openSUSE, Fedora)
    "/usr/lib/clang"
    PATH_SUFFIXES ${LIBCLANG_KNOWN_LLVM_VERSIONS})

  if (CHECK_CLANG_RESOURCE_DIR)
    # On Windows the paths weren't escaped correctly, similar to:
    # http://public.kitware.com/pipermail/cmake/2006-February/008473.html
    list(APPEND run_args -resource-dir \"${CHECK_CLANG_RESOURCE_DIR}\")
  endif()

  list(APPEND run_args ${checked_file})

  try_run(
    CHECK_CLANG_RESOURCE_DIR_NUM_DIAGNOSTICS
    CHECK_CLANG_RESOURCE_DIR_COMPILE_RESULT
    ${CMAKE_BINARY_DIR}
    ${checker_code}
    CMAKE_FLAGS
      "-DINCLUDE_DIRECTORIES:STRING=${LIBCLANG_INCLUDE_DIRS}"
      "-DLINK_LIBRARIES:STRING=${LIBCLANG_LIBRARIES}"
    COMPILE_OUTPUT_VARIABLE compile_output
    RUN_OUTPUT_VARIABLE run_output
    ARGS ${run_args}
    )

  if (NOT CHECK_CLANG_RESOURCE_DIR_COMPILE_RESULT)
    set(CHECK_CLANG_RESOURCE_DIR_NUM_DIAGNOSTICS 1)
  endif()

  if (CHECK_CLANG_RESOURCE_DIR_NUM_DIAGNOSTICS EQUAL 0)
    message(STATUS "Detecting libclang builtin headers directory -- success")
    if (CHECK_CLANG_RESOURCE_DIR)
      set(CLANG_RESOURCE_DIR "${CHECK_CLANG_RESOURCE_DIR}"
        CACHE INTERNAL "Clang resource directory.")
    endif()
  else()
    message(STATUS "Detecting Clang resource directory -- fail")

    if (NOT CHECK_CLANG_RESOURCE_DIR_COMPILE_RESULT)
      message(WARNING "CheckClangResourceDir: failed to compile checker, please report.
  Compile output:
    ${compile_output}
")
    else()
      message(WARNING "CheckClangResourceDir: unsupported configuration, please report.

  Check with args: ${run_args}
  Check output:
    ${run_output}
")
    endif()
  endif()
endfunction()
