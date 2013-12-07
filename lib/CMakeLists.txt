#
# irony-utils library build file
#

include_directories (${IRONY_UTILS_INCLUDES})

add_library (irony-utils
  # utils sources
  util/arraysize.h
  util/NonCopyable.h
  util/JSONObjectWrapper.cpp
  util/JSONObjectWrapper.h

  # SimpleJSON sources
  SimpleJSON/src/JSON.cpp
  SimpleJSON/src/JSON.h
  SimpleJSON/src/JSONValue.cpp
  SimpleJSON/src/JSONValue.h

  # String sources
  str/wstring_to_string.cpp
  str/wstring_to_string.h
  str/to_string.h
  )
