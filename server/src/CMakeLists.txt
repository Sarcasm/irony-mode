find_package(LibClang REQUIRED)

include_directories(${LIBCLANG_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_executable(irony-server
  support/arraysize.h
  support/CommandLineParser.cpp
  support/CommandLineParser.h
  support/iomanip_quoted.h
  support/NonCopyable.h
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

target_link_libraries(irony-server ${LIBCLANG_LIBRARIES})

install(TARGETS irony-server DESTINATION bin)
