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

# Copied/Pasted (or killed/yanked if you (I) prefer) from the LLVM
# CMakeLists.txt
if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR AND NOT MSVC_IDE)
  message(FATAL_ERROR "In-source builds are not allowed.

  Please create a build/ directory and run cmake from there, passing
  the path to this source directory as the last argument. This process
  created the file `CMakeCache.txt' and the directory `CMakeFiles'.
  Please delete them.")
endif()

set (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

# Add some extra flags during cxx compilation
if (CMAKE_COMPILER_IS_GNUCXX OR ${CMAKE_CXX_COMPILER} MATCHES "clang\\+\\+")
  # The clang c++ compiler driver is compatible with g++ (same flags)
  set (CMAKE_CXX_FLAGS "-Wall -Wextra -Wshadow ${CMAKE_CXX_FLAGS}")
endif()

# Don't forget to add dependencies for new executable
# add_dependencies (executable lib)
set (IRONY_UTILS_INCLUDES
  ${IRONY_SOURCE_DIR}/lib
  ${IRONY_SOURCE_DIR}/lib/SimpleJSON/src)

add_subdirectory (lib)
add_subdirectory (server)
# add_subdirectory (test)
