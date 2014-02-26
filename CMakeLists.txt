#
# Compilation instructions:
#
# Builds in a separate build/ directory:
#       mkdir build
#       cd build
#       cmake ..
#
# If you want to build with Clang, in the last step you can try:
#       env CXX=clang++ cmake ..
#
# You can specify the build type:
#       cmake -DCMAKE_BUILD_TYPE=[Release|Debug] ..
#

# The name of our project is IRONY.
# CMakeLists files in this project can refer to the root source
# directory of the project as # ${IRONY_SOURCE_DIR} and to the root
# binary directory of the project as ${IRONY_BINARY_DIR}.
cmake_minimum_required(VERSION 2.8)
project(IRONY)

set (CMAKE_MODULE_PATH
  ${IRONY_SOURCE_DIR}/cmake
  ${IRONY_SOURCE_DIR}/cmake/modules
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

set (IRONY_UTILS_INCLUDES
  ${IRONY_SOURCE_DIR}/lib
  ${IRONY_SOURCE_DIR}/lib/SimpleJSON/src
  )

# Don't forget to add dependencies for new executable
# add_dependencies (executable lib)
add_subdirectory (lib)
add_subdirectory (server)
add_subdirectory (docs)
enable_testing ()
add_subdirectory (test)
