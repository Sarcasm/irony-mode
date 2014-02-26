#
# Try to find libclang
#
# Once done this will define:
# - LIBCLANG_FOUND
#               System has libclang
# - LIBCLANG_INCLUDE_DIRS
#               The libclang include directories
# - LIBCLANG_LIBRARIES
#               The libraries needed to use libclang
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

find_path (LIBCLANG_INCLUDE_DIR clang-c/Index.h
  HINTS ${LIBCLANG_INCLUDE_PATH}
  PATHS
  # LLVM Debian/Ubuntu nightly packages: http://llvm.org/apt/
  /usr/lib/llvm-3.1/include/
  /usr/lib/llvm-3.2/include/
  /usr/lib/llvm-3.3/include/
  /usr/lib/llvm-3.4/include/
  /usr/lib/llvm-3.5/include/
  # LLVM MacPorts
  /opt/local/libexec/llvm-3.1/include
  /opt/local/libexec/llvm-3.2/include
  /opt/local/libexec/llvm-3.3/include
  /opt/local/libexec/llvm-3.4/include
  /opt/local/libexec/llvm-3.5/include
  # LLVM Homebrew
  /usr/local/Cellar/llvm/3.1/include
  /usr/local/Cellar/llvm/3.2/include
  /usr/local/Cellar/llvm/3.3/include
  /usr/local/Cellar/llvm/3.4/include
  /usr/local/Cellar/llvm/3.5/include
  )

find_library (LIBCLANG_LIBRARY NAMES clang libclang
  HINTS ${LIBCLANG_LIBRARY_PATH}
  PATHS
  # LLVM Debian/Ubuntu nightly packages: http://llvm.org/apt/
  /usr/lib/llvm-3.1/lib/
  /usr/lib/llvm-3.2/lib/
  /usr/lib/llvm-3.3/lib/
  /usr/lib/llvm-3.4/lib/
  /usr/lib/llvm-3.5/lib/
  # LLVM MacPorts
  /opt/local/libexec/llvm-3.1/lib
  /opt/local/libexec/llvm-3.2/lib
  /opt/local/libexec/llvm-3.3/lib
  /opt/local/libexec/llvm-3.4/lib
  /opt/local/libexec/llvm-3.5/lib
  # LLVM Homebrew
  /usr/local/Cellar/llvm/3.1/lib
  /usr/local/Cellar/llvm/3.2/lib
  /usr/local/Cellar/llvm/3.3/lib
  /usr/local/Cellar/llvm/3.4/lib
  /usr/local/Cellar/llvm/3.5/lib
  # LLVM Fedora
  /usr/lib/llvm
  )

set (LIBCLANG_LIBRARIES ${LIBCLANG_LIBRARY})
set (LIBCLANG_INCLUDE_DIRS ${LIBCLANG_INCLUDE_DIR})

include (FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LIBCLANG_FOUND to TRUE if
# all listed variables are TRUE
find_package_handle_standard_args (LibClang DEFAULT_MSG
  LIBCLANG_LIBRARY LIBCLANG_INCLUDE_DIR)

mark_as_advanced (LIBCLANG_INCLUDE_DIR LIBCLANG_LIBRARY)
