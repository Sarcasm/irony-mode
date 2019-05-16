# XXX: if there are issues
# due to https://reviews.llvm.org/D30911 / SVN Revision 298424
# then use the fallback when LLVM_VERSION VERSION_LESS 5.0.1
find_package(Clang)

if (Clang_FOUND)
  # XXX: at least since Clang 8.0.0
  # it's looks like the 'libclang' IMPORTED target
  # does not specify the following target property:
  #     INTERFACE_INCLUDE_DIRECTORIES ${CLANG_INCLUDE_DIRS}
  # which is confirmed by https://github.com/Sarcasm/irony-mode/issues/530
  # so we work around this,
  # but ideally Clang upstream should export fully usable IMPORTED targets
  add_library(irony_libclang INTERFACE)
  target_link_libraries(irony_libclang INTERFACE libclang)
  target_include_directories(irony_libclang INTERFACE ${CLANG_INCLUDE_DIRS})

  get_property(IRONY_CLANG_EXECUTABLE TARGET clang PROPERTY LOCATION)
  execute_process(
    COMMAND "${IRONY_CLANG_EXECUTABLE}" -print-resource-dir
    OUTPUT_VARIABLE CLANG_RESOURCE_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
else()
  # fallback to historically FindLibClang.cmake
  # and -resource-dir guess mecanism
  include(CheckClangResourceDir)
  find_package(LibClang REQUIRED)
  check_clang_resource_dir()
endif()

# not to be taken as a module-definition file to link on Windows
set_source_files_properties(Commands.def PROPERTIES HEADER_FILE_ONLY TRUE)

if(MSVC)
  # irony-server uses some code that breaks when iterator debugging is enabled
  #
  # The culprit is CommandLineArgumentParser who initialize its member
  # 'Position', of type 'std::string::const_iterator', to 'Input.begin() - 1'.
  # With checked iterator the begin() - 1 breaks in debug build.
  add_definitions(/D_ITERATOR_DEBUG_LEVEL=0)
endif()

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
  CompDBCache.h
  CompDBCache.cpp
  Irony.cpp
  Irony.h
  TUManager.cpp
  TUManager.h

  main.cpp)

irony_target_set_cxx_standard(irony-server)

target_include_directories(irony-server PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

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

if (CLANG_RESOURCE_DIR)
  # look for CLANG_RESOURCE_DIR_DIR usage in the code for an explanation
  set_source_files_properties(TUManager.cpp
    PROPERTIES
    COMPILE_DEFINITIONS CLANG_RESOURCE_DIR=\"${CLANG_RESOURCE_DIR}\")
endif()

target_link_libraries(irony-server irony_libclang)

set_target_properties(irony-server
  PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})

install(TARGETS irony-server DESTINATION ${CMAKE_INSTALL_BINDIR})
