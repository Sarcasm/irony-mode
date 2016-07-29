cmake_minimum_required(VERSION 2.8.3) # CMAKE_CURRENT_LIST_DIR

project(IronyMode)

set(CMAKE_MODULE_PATH
  ${PROJECT_SOURCE_DIR}/cmake
  ${PROJECT_SOURCE_DIR}/cmake/modules
  ${CMAKE_MODULE_PATH})

include(utils)

include(CTest)

check_for_in_source_build()
release_as_default_build_type()

# Disable exception, we aren't using them and right now clang-cl needs them
# disabled to parse Windows headers.
#
# Logic stolen from LLVM
if (CMAKE_COMPILER_IS_GNUCXX)
  set(IRONY_COMPILER_IS_GCC_COMPATIBLE ON)
elseif (MSVC)
  set(IRONY_COMPILER_IS_GCC_COMPATIBLE OFF)
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(IRONY_COMPILER_IS_GCC_COMPATIBLE ON)
endif()

if(IRONY_COMPILER_IS_GCC_COMPATIBLE)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions")
elseif(MSVC)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHs-c- /D_HAS_EXCEPTIONS=0")
  # irony-server uses some code that breaks when iterator debugging is enabled
  #
  # The culprit is CommandLineArgumentParser who initialize its member
  # 'Position', of type 'std::string::const_iterator', to 'Input.begin() - 1'.
  # With checked iterator the begin() - 1 breaks in debug build.
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /D_ITERATOR_DEBUG_LEVEL=0")
endif()

enable_colored_diagnotics()
check_cxx11_options()

if (CXX11_COMPILE_OPTIONS)
  add_compile_options_(${CXX11_COMPILE_OPTIONS})
endif()

foreach (link_option ${CXX11_LINK_OPTIONS})
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${link_option}")
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${link_option}")
  set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${link_option}")
endforeach()

if (CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  add_compile_options_(-Wall -Wextra)
endif()

option(GENERATE_DOXYGEN "Whether or not to build the Doxygen documentation" OFF)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_subdirectory(src)
add_subdirectory(docs)

if (BUILD_TESTING)
  add_subdirectory(test)
endif()
