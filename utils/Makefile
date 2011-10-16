# General configuration settings

LIBNAME = libutils.a
DESTDIR = ..
OUTPUT = $(DESTDIR)/$(LIBNAME)

# Compiler
# Compilation with Clang++ could be done with:
# CXX=clang++ CC=clang make [options]
CXX ?= g++
CC ?= gcc

LIB_JSON_SRC_DIR = SimpleJSON/src
LIB_JSON_INCLUDES_DIR = SimpleJSON/src

SRCS := $(wildcard *.cpp) \
	$(LIB_JSON_SRC_DIR)/JSON.cpp \
	$(LIB_JSON_SRC_DIR)/JSONValue.cpp

OBJS := $(SRCS:%.cpp=%.o)

INCLUDES = -I$(LIB_JSON_INCLUDES_DIR)

# Could add optimisation flags (say -O2)
CXXFLAGS = -Wall -Wextra -Wshadow -Wpointer-arith $(INCLUDES)
CFLAGS = -Wall -Wextra -Wshadow -ansi -pedantic
LDFLAGS =
DEBUG = -ggdb3

# Archiver
AR = ar
ARFLAGS = rcs

# Others
RM = rm -f
CP = cp

.PHONY: all check-syntax gprof clean cleanall debug all

all: $(OUTPUT)

$(OUTPUT): $(OBJS)
	$(AR) $(ARFLAGS) $(OUTPUT) $(OBJS)

clean:
	$(RM) $(OBJS)

cleanall: clean
	$(RM) $(OUTPUT)

debug: CXXFLAGS += $(DEBUG)
debug: all

%.o	: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ -c $<

check-syntax:
		g++ $(CXXFLAGS) -fsyntax-only $(CHK_SOURCES)
