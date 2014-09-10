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
# - LIBCLANG_LIBRARY_DIR
#               The path to the directory containing libclang.
# - LIBCLANG_KNOWN_LLVM_VERSIONS
#               Known LLVM release numbers.
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

# most recent versions come first
set(LIBCLANG_KNOWN_LLVM_VERSIONS 3.6
  3.5.0                   #Arch Linux
  3.5                     #LLVM Debian/Ubuntu packages from http://llvm.org/apt/
  3.4.2 3.4.1 3.4 3.3 3.2 3.1)

set(libclang_llvm_header_search_paths)
set(libclang_llvm_lib_search_paths
  # LLVM Fedora
  /usr/lib/llvm
  )

foreach (version ${LIBCLANG_KNOWN_LLVM_VERSIONS})
  list(APPEND libclang_llvm_header_search_paths
    # LLVM Debian/Ubuntu nightly packages: http://llvm.org/apt/
    "/usr/lib/llvm-${version}/include/"
    # LLVM MacPorts
    "/opt/local/libexec/llvm-${version}/include"
    # LLVM Homebrew
    "/usr/local/Cellar/llvm/${version}/include"
    # LLVM Homebrew/versions
    "/usr/local/lib/llvm-${version}/include"
    )

  list(APPEND libclang_llvm_lib_search_paths
    # LLVM Debian/Ubuntu nightly packages: http://llvm.org/apt/
    "/usr/lib/llvm-${version}/lib/"
    # LLVM MacPorts
    "/opt/local/libexec/llvm-${version}/lib"
    # LLVM Homebrew
    "/usr/local/Cellar/llvm/${version}/lib"
    # LLVM Homebrew/versions
    "/usr/local/lib/llvm-${version}/lib"
    )
endforeach()

find_path(LIBCLANG_INCLUDE_DIR clang-c/Index.h
  HINTS ${LIBCLANG_INCLUDE_PATH}
  PATHS ${libclang_llvm_header_search_paths})

find_library(LIBCLANG_LIBRARY NAMES clang libclang
  HINTS ${LIBCLANG_LIBRARY_PATH}
  PATHS ${libclang_llvm_lib_search_paths})

get_filename_component(LIBCLANG_LIBRARY_DIR ${LIBCLANG_LIBRARY} PATH)

set(LIBCLANG_LIBRARIES ${LIBCLANG_LIBRARY})
set(LIBCLANG_INCLUDE_DIRS ${LIBCLANG_INCLUDE_DIR})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LIBCLANG_FOUND to TRUE if
# all listed variables are TRUE
find_package_handle_standard_args(LibClang DEFAULT_MSG
  LIBCLANG_LIBRARY LIBCLANG_INCLUDE_DIR)

mark_as_advanced(LIBCLANG_INCLUDE_DIR LIBCLANG_LIBRARY)
