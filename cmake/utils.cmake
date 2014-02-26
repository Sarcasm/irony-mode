include(CheckCXXCompilerFlag)

#
# check_for_in_source_build()
#
function(check_for_in_source_build)
  if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR AND NOT MSVC_IDE)
    message(FATAL_ERROR "In-source builds are not allowed.

  Please create a build/ directory and run cmake from there, passing
  the path to this source directory as the last argument. This process
  created the file `CMakeCache.txt' and the directory `CMakeFiles'.
  Please delete them.
")
  endif()
endfunction()

#
# release_as_default_build_type()
#
function(release_as_default_build_type)
  # Set a default build type if none was specified for build systems with a
  # unique configuration type (i.e: Make/Ninja builds)
  if (NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
    message(STATUS "Setting build type to 'Release' as none was specified")
    set (CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
    set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
      "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
  endif()
endfunction()

#
# add_compile_options_(<opts...>)
#
# Adds options to the compiler command line for sources in the current directory
# and below.
#
# note: add_compile_options() backport, which first appeared in CMake 2.8.12
function(add_compile_options_)
  # possible to check with:
  #   if (${CMAKE_VERSION} VERSION_LESS "2.8.12")
  if (COMMAND add_compile_options)
    add_compile_options(${ARGN})
  else()
    add_definitions(${ARGN})
  endif()
endfunction()

#
# enable_colored_diagnotics()
#
# Setup the flag to enable colored diagnostics if any.
#
# For now this option is enforced only for Ninja builds, where compiler output
# is redirected to pipes.
#
# Clang has '-fcolor-diagnostics' for a long time now. Since GCC 4.9, a similar
# flag has been added '-fdiagnostics-color' (somehow they managed to use another
# syntax than Clang's one...). Recent version of Clang will support both as they
# added support for GCC's -fdiagnostics-color.
#
function(enable_colored_diagnotics)
  if (${CMAKE_GENERATOR} MATCHES "Ninja")
    # Clang
    check_cxx_compiler_flag("-fcolor-diagnostics" HAS_FCOLOR_DIAGNOSTICS_FLAG)
    if (HAS_FCOLOR_DIAGNOSTICS_FLAG)
      add_compile_options_(-fcolor-diagnostics)
    else()                      # GCC (and Clang for compatibility with GCC)
      check_cxx_compiler_flag("-fdiagnostics-color" HAS_DIAGNOSTICS_FCOLOR_FLAG)
      if (HAS_DIAGNOSTICS_FCOLOR_FLAG)
        add_compile_options_(-fdiagnostics-color)
      endif()
    endif()
  endif()
endfunction()
