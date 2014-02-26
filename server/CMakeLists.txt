#
# irony-server executable build file
#
find_package (LibClang REQUIRED)

include_directories (${LIBCLANG_INCLUDE_DIRS})
include_directories (${IRONY_UTILS_INCLUDES})
include_directories (${CMAKE_CURRENT_SOURCE_DIR})

#
# irony-server executable
#
add_executable (irony-server
  # Main sources
  IPlugin.cpp
  IPlugin.h
  ClangString.cpp
  ClangString.h
  Server.cpp
  Server.h
  TUManager.cpp
  TUManager.h
  main.cpp

  # "Built-in" plugins sources
  plugins/CodeCompletion.cpp
  plugins/CodeCompletion.h
  plugins/SyntaxChecker.cpp
  plugins/SyntaxChecker.h
  plugins/CacheInvalidation.h
  plugins/CacheInvalidation.cpp
  plugins/CompileChecker.cpp
  plugins/CompileChecker.h
  )

add_dependencies (irony-server
  # Dependencies
  irony-utils
  )

target_link_libraries (irony-server
  irony-utils
  ${LIBCLANG_LIBRARIES}
  )

install (TARGETS irony-server
  RUNTIME DESTINATION ${IRONY_SOURCE_DIR}/bin)
