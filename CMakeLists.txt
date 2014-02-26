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

# If Clang specific code is not used it will be a better idea to use:
#       if (CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
if ( "${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang" )
  set(CMAKE_COMPILER_IS_CLANGCXX TRUE)
endif()

# libclang in non standard locations can cause some troubles.
option(USE_RPATH "Enable rpath for shared libraries (such as libclang.so)." OFF)

if (USE_RPATH)
  set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
endif()

# Add some extra flags during C++ compilation (targets GCC and Clang
# compatible flags).
if (CMAKE_COMPILER_IS_GNUCXX OR CMAKE_COMPILER_IS_CLANGCXX)
  include(CheckCXXCompilerFlag)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wextra -Wall -Wshadow")

  # the flags -fcolor-diagnostics is for Ninja build, to get colored
  # output from Clang.
  if (${CMAKE_GENERATOR} MATCHES "Ninja")
    check_cxx_compiler_flag("-fcolor-diagnostics" HAS_FCOLOR_DIAGNOSTICS_FLAG)
    if (HAS_FCOLOR_DIAGNOSTICS_FLAG)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fcolor-diagnostics")
    endif()
  endif ()
endif ()

set (IRONY_UTILS_INCLUDES
  ${IRONY_SOURCE_DIR}/lib
  ${IRONY_SOURCE_DIR}/lib/SimpleJSON/src)

# Don't forget to add dependencies for new executable
# add_dependencies (executable lib)
add_subdirectory (lib)
add_subdirectory (server)
add_subdirectory (docs)
enable_testing ()
add_subdirectory (test)
