# Find libclang.
#
# This module defines the following variables:
# LIBCLANG_FOUND - true if libclang has been found and can be used
# LIBCLANG_KNOWN_LLVM_VERSIONS - known LLVM release numbers
# LIBCLANG_INCLUDE_DIRS - the libclang include directories
# LIBCLANG_LIBRARIES - the libraries needed to use libclang
# LIBCLANG_LIBRARY_DIR - the path to the directory containing libclang
#
# This module defines the following IMPORTED target:
# - irony_libclang

# most recent versions come first
# http://llvm.org/apt/
set(LIBCLANG_KNOWN_LLVM_VERSIONS 9.0.0 9.0 9
  8.0.0 8.0 8
  7.0.1 7.0.0 7.0 7
  6.0.1 6.0.0 6.0 6
  5.0.2 5.0.1 5.0.0 5.0 5
  4.0.1 4.0.0 4.0 4
  3.9.1 3.9.0 3.9
  3.8.1 3.8.0 3.8
  3.7.1 3.7.0 3.7
  3.6.2 3.6.1 3.6.0 3.6
  3.5.2 3.5.1 3.5.0 3.5
  3.4.2 3.4.1 3.4
  3.3
  3.2
  3.1)

set(libclang_llvm_header_search_paths)
set(libclang_llvm_lib_search_paths
  # LLVM Fedora
  /usr/lib/llvm
  )

foreach (version ${LIBCLANG_KNOWN_LLVM_VERSIONS})
  string(REPLACE "." "" undotted_version "${version}")
  list(APPEND libclang_llvm_header_search_paths
    # LLVM Debian/Ubuntu nightly packages: http://llvm.org/apt/
    "/usr/lib/llvm-${version}/include/"
    # LLVM MacPorts
    "/opt/local/libexec/llvm-${version}/include"
    # LLVM Homebrew
    "/usr/local/Cellar/llvm/${version}/include"
    # LLVM Homebrew/versions
    "/usr/local/lib/llvm-${version}/include"
    # FreeBSD ports versions
    "/usr/local/llvm${undotted_version}/include"
    # Gentoo clang-4
    "/usr/lib/llvm/${version}/include"
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
    # FreeBSD ports versions
    "/usr/local/llvm${undotted_version}/lib"
    # Gentoo clang-4
    "/usr/lib/llvm/${version}/lib"
    )
endforeach()

find_path(LIBCLANG_INCLUDE_DIR clang-c/Index.h
  PATHS ${libclang_llvm_header_search_paths}
  PATH_SUFFIXES LLVM/include #Windows package from http://llvm.org/releases/
  DOC "The path to the directory that contains clang-c/Index.h")

find_library(LIBCLANG_LIBRARY
  NAMES
    # On Windows with MSVC, the import library uses the ".imp" file extension
    # instead of the comon ".lib"
    libclang.imp
    libclang
    clang
  PATHS ${libclang_llvm_lib_search_paths}
  PATH_SUFFIXES LLVM/lib #Windows package from http://llvm.org/releases/
  DOC "The file that corresponds to the libclang library.")

get_filename_component(LIBCLANG_LIBRARY_DIR ${LIBCLANG_LIBRARY} PATH)

set(LIBCLANG_LIBRARIES ${LIBCLANG_LIBRARY})
set(LIBCLANG_INCLUDE_DIRS ${LIBCLANG_INCLUDE_DIR})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LIBCLANG_FOUND to TRUE if
# all listed variables are TRUE
find_package_handle_standard_args(LibClang DEFAULT_MSG
  LIBCLANG_LIBRARY LIBCLANG_INCLUDE_DIR)

mark_as_advanced(LIBCLANG_INCLUDE_DIR LIBCLANG_LIBRARY)

if (LIBCLANG_FOUND AND NOT TARGET irony_libclang)
  add_library(irony_libclang UNKNOWN IMPORTED)
  set_target_properties(irony_libclang PROPERTIES
    IMPORTED_LINK_INTERFACE_LANGUAGES "CXX"
    IMPORTED_LOCATION "${LIBCLANG_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${LIBCLANG_INCLUDE_DIR}"
  )
endif()
