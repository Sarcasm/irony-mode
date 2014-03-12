cmake_minimum_required(VERSION 2.8)

project(IronyMode)

set(CMAKE_MODULE_PATH
  ${PROJECT_SOURCE_DIR}/cmake
  ${PROJECT_SOURCE_DIR}/cmake/modules
  ${CMAKE_MODULE_PATH})

include(utils)

check_for_in_source_build()
release_as_default_build_type()
enable_colored_diagnotics()

if (CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  add_compile_options_(-Wall -Wextra)
endif()

# libclang in non standard locations can cause some troubles.
option(USE_RPATH "Enable rpath for shared libraries (such as libclang.so)." OFF)

if (USE_RPATH)
  set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
endif()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

set(IRONY_UTILS_INCLUDES
  ${PROJECT_SOURCE_DIR}/lib
  ${PROJECT_SOURCE_DIR}/lib/SimpleJSON/src
  )

# Don't forget to add dependencies for new executable
# add_dependencies (executable lib)
add_subdirectory(lib)
add_subdirectory(server)
add_subdirectory(docs)
enable_testing()
add_subdirectory(test)
