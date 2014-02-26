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
