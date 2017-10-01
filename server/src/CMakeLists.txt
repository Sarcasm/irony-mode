include(CheckClangResourceDir)

find_package(LibClang REQUIRED)

include_directories(${LIBCLANG_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

check_clang_resource_dir()

if (CLANG_RESOURCE_DIR)
  # look for CLANG_BUILTIN_HEADERS_DIR usage in the code for an explanation
  add_definitions(-DCLANG_RESOURCE_DIR=\"${CLANG_RESOURCE_DIR}\")
endif()

# not to be taken as a module-definition file to link on Windows
set_source_files_properties(Commands.def PROPERTIES HEADER_FILE_ONLY TRUE)

add_executable(irony-server
  support/CommandLineParser.cpp
  support/CommandLineParser.h
  support/iomanip_quoted.h
  support/NonCopyable.h
  support/CIndex.h
  support/TemporaryFile.cpp
  support/TemporaryFile.h

  Command.cpp
  Commands.def
  Command.h
  Irony.cpp
  Irony.h
  TUManager.cpp
  TUManager.h

  main.cpp)

# retrieve the package version from irony.el
function(irony_find_package_version OUTPUT_VAR)
  # this is a hack that force CMake to reconfigure, it is necessary to see if
  # the version in irony.el has changed, this is not possible to add the
  # definitions at build time
  configure_file(${PROJECT_SOURCE_DIR}/../irony.el
    ${CMAKE_CURRENT_BINARY_DIR}/irony.el
    COPYONLY)

  set(version_header "\;\; Version: ")
  file(STRINGS ${CMAKE_CURRENT_BINARY_DIR}/irony.el version
    LIMIT_COUNT 1
    REGEX "^${version_header}*")

  if (NOT version)
    message (FATAL_ERROR "couldn't find irony.el's version header!")
  endif()

  string(LENGTH ${version_header} version_header_length)
  string(SUBSTRING ${version} ${version_header_length} -1 package_version)
  set(${OUTPUT_VAR} ${package_version} PARENT_SCOPE)
endfunction()

irony_find_package_version(IRONY_PACKAGE_VERSION)
message(STATUS "Irony package version is '${IRONY_PACKAGE_VERSION}'")

set_source_files_properties(main.cpp
  PROPERTIES
  COMPILE_DEFINITIONS IRONY_PACKAGE_VERSION=\"${IRONY_PACKAGE_VERSION}\")

target_link_libraries(irony-server ${LIBCLANG_LIBRARIES})

install(TARGETS irony-server DESTINATION bin)
