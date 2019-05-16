cmake_minimum_required(VERSION 3.0.2)

project(IronyMode)

set(CMAKE_MODULE_PATH
  ${PROJECT_SOURCE_DIR}/cmake
  ${PROJECT_SOURCE_DIR}/cmake/modules
  ${CMAKE_MODULE_PATH})

include(CTest)
include(GNUInstallDirs)

# Starting from CMake >= 3.1, if a specific standard is required,
# it can be set from the command line with:
#     cmake -DCMAKE_CXX_STANDARD=[11|14|17]
function(irony_target_set_cxx_standard target)
  set(cxx_standard 11)
  if (CMAKE_VERSION VERSION_LESS "3.1")
    if (CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
      target_compile_options(${target} PRIVATE -std=c++${cxx_standard})
    endif()
  elseif (CMAKE_VERSION VERSION_LESS "3.8")
    set_property(TARGET ${target} PROPERTY CXX_STANDARD ${cxx_standard})
  else()
    target_compile_features(${target} PUBLIC cxx_std_${cxx_standard})
  endif()
endfunction()

add_subdirectory(src)

if (BUILD_TESTING)
  add_subdirectory(test)
endif()
