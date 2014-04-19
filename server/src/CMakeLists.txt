find_package(LibClang REQUIRED)

include_directories(${LIBCLANG_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_executable(irony-server
  support/arraysize.h
  support/NonCopyable.h
  support/TemporaryFile.h
  support/TemporaryFile.cpp

  ClangString.cpp
  ClangString.h
  Command.cpp
  Commands.def
  Command.h
  Irony.cpp
  Irony.h
  TUManager.cpp
  TUManager.h

  main.cpp)

target_link_libraries(irony-server ${LIBCLANG_LIBRARIES})

install(TARGETS irony-server DESTINATION bin)
