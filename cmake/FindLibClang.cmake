#
# Try to find LibClang
#
# Once done this will define:
# - LibClang_FOUND
#               System has LibXml2
# - LibClang_INCLUDE_DIRS
#               The LibXml2 include directories
# - LibClang_LIBRARIES
#               The libraries needed to use LibXml2
# - LibClang_DEFINITIONS
#               Compiler switches required for using LibXml2
#
# If a pkg-config is available for libclang one day...
# find_package(PkgConfig)
# pkg_check_modules(PC_LibClang QUIET libclang)
# set(LibClang_DEFINITIONS ${PC_LibClang_CFLAGS_OTHER})

find_path (LibClang_INCLUDE_DIR clang-c/Index.h
  # HINTS ${PC_LibClang_INCLUDEDIR} ${PC_LibClang_INCLUDE_DIRS}
  )

find_library (LibClang_LIBRARY NAMES clang libclang
  # HINTS ${PC_LibClang_LIBDIR} ${PC_LibClang_LIBRARY_DIRS}
  PATHS
  /usr/lib/llvm                 # for *Arch?* Linux
  )

set (LibClang_LIBRARIES ${LibClang_LIBRARY})
set (LibClang_INCLUDE_DIRS ${LibClang_INCLUDE_DIR})

include (FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LibClang_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args (LibClang DEFAULT_MSG
  LibClang_LIBRARY LibClang_INCLUDE_DIR)

mark_as_advanced (LibClang_INCLUDE_DIR LibClang_LIBRARY)
