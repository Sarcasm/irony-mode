include(CheckCXXCompilerFlag)
include(CheckCXXSourceCompiles)

#
# check_cxx11_options()
#
# Throws a FATAL_ERROR if C++11 isn't available otherwise sets:
# - CXX11_COMPILE_OPTIONS
# - CXX11_LINK_OPTIONS
#
function(check_cxx11_options)
  if (CXX11_COMPILE_OPTIONS)
    return() # already in cache
  endif()

  if (CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    check_cxx_compiler_flag("-std=c++11" HAS_STDCXX11)
    if (HAS_STDCXX11)
      set(compile_options -std=c++11)
    else()
      check_cxx_compiler_flag("-std=c++0x" HAS_STDCXX0X)
      if (HAS_STDCXX0X)
        set(compile_options -std=c++0x)
      endif()
    endif()

    # Check whether or not the system library provides proper C++11 support, if
    # not we try to link specifically against libc++.
    #
    # This seems useful for Mac OS X builds, see:
    # http://cplusplusmusings.wordpress.com/2012/07/05/clang-and-standard-libraries-on-mac-os-x/
    set(CMAKE_REQUIRED_FLAGS ${compile_options})
    check_cxx_source_compiles("#include <random>

int main() {
    std::random_device rd;
    std::default_random_engine e(rd());
    std::uniform_int_distribution<int> dist(0, 15);

    return dist(e);
}
"
      HAS_CXX11_STDLIB)

    if (NOT HAS_CXX11_STDLIB)
      check_cxx_compiler_flag("-stdlib=libc++" HAS_LIBCXX)

      if (HAS_LIBCXX)
        list(APPEND compile_options -stdlib=libc++)
        list(APPEND link_options -stdlib=libc++)
      else()
        message(FATAL_ERROR "Standard library doesn't support C++11!")
      endif()
    endif()
  endif()

  if (compile_options)
    message(STATUS "C++11 compiler option(s): ${compile_options}")
  endif()

  set(CXX11_COMPILE_OPTIONS ${compile_options} CACHE INTERNAL
    "C++11 compile options, if any")
  set(CXX11_LINK_OPTIONS ${link_options} CACHE INTERNAL
    "C++11 link options, if any")
endfunction()
