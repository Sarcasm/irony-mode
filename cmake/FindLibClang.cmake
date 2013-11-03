#
# Try to find LibClang
#
# Once done this will define:
# - LibClang_FOUND
#               System has libclang
# - LibClang_INCLUDE_DIRS
#               The Libclang include directories
# - LibClang_LIBRARIES
#               The libraries needed to use libclang
# - LibClang_DEFINITIONS
#               Compiler switches required for using libclang
#
# At the CMake invocation level it is possible to specify some hints for the
# libclang installation, e.g: for non-standard libclang installations.
#
# To specify the include directory use:
#   -DLIBCLANG_INCLUDE_PATH=/path/to/libclang/include-dir
# The specified directory should contain the header file 'clang-c/Index.h'
#
# To specify the library directory use:
#   -DLIBCLANG_LIBRARY_PATH=/path/to/libclang/libraries
# The specified directory should contain the libclang library, e.g: libclang.so
# on Linux.
#
# CMake invocation example with a custom libclang installation:
#     cmake -DLIBCLANG_INCLUDE_PATH=~/llvm-3.4/include/ \
#           -DLIBCLANG_LIBRARY_PATH=~/llvm-3.4/lib/ <args...>

find_path (LibClang_INCLUDE_DIR clang-c/Index.h
  HINTS ${LIBCLANG_INCLUDE_PATH}
  PATHS
  # LLVM Debian/Ubuntu nightly packages: http://llvm.org/apt/
  /usr/lib/llvm-3.1/include/
  /usr/lib/llvm-3.2/include/
  /usr/lib/llvm-3.3/include/
  /usr/lib/llvm-3.4/include/
  )

find_library (LibClang_LIBRARY NAMES clang libclang
  HINTS ${LIBCLANG_LIBRARY_PATH}
  PATHS
  # LLVM Debian/Ubuntu nightly packages: http://llvm.org/apt/
  /usr/lib/llvm-3.1/lib/
  /usr/lib/llvm-3.2/lib/
  /usr/lib/llvm-3.3/lib/
  /usr/lib/llvm-3.4/lib/
  )

set (LibClang_LIBRARIES ${LibClang_LIBRARY})
set (LibClang_INCLUDE_DIRS ${LibClang_INCLUDE_DIR})

include (FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LibClang_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args (LibClang DEFAULT_MSG
  LibClang_LIBRARY LibClang_INCLUDE_DIR)

mark_as_advanced (LibClang_INCLUDE_DIR LibClang_LIBRARY)
