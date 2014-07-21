cmake_minimum_required(VERSION 2.8.3) # CMAKE_CURRENT_LIST_DIR

project(IronyMode)

set(CMAKE_MODULE_PATH
  ${PROJECT_SOURCE_DIR}/cmake
  ${PROJECT_SOURCE_DIR}/cmake/modules
  ${CMAKE_MODULE_PATH})

include(utils)

check_for_in_source_build()
release_as_default_build_type()
enable_colored_diagnotics()
enable_cxx11_compile_options()

if (CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  add_compile_options_(-Wall -Wextra)
endif()

option(GENERATE_DOXYGEN "Whether or not to build the Doxygen documentation" OFF)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_subdirectory(src)
add_subdirectory(docs)
enable_testing()
add_subdirectory(test)
