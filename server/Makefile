#
# This file is distributed under the GNU General Public License. See
# COPYING for details.
#
# General configuration settings

EXEC = irony-server

DESTDIR = ..

JSON_INCLUDES_DIR = ../utils/SimpleJSON/src

# Compiler
# Compilation with clang++ could be done with:
# CXX=clang++ [options]
CXX ?= g++

SRCS := $(wildcard *.cpp) \
	$(wildcard plugins/*.cpp)

OBJS = $(SRCS:%.cpp=%.o)

# INCLUDES='-I /PATH/TO/CLANG-C/../DIRECTORY' make
INCLUDES += -I. -I../utils -I.. -I$(JSON_INCLUDES_DIR)

CXXFLAGS += -Wall -Wextra -Wshadow -Wpointer-arith $(INCLUDES)
LLVM_LIB := -L /usr/lib/llvm # Arch Linux see /etc/ld.so.conf.d/llvm.conf
LDFLAGS += -L.. -lutils -lclang $(LLVM_LIB)
DEBUG = -ggdb3

# Others
RM = rm -f
CP = cp

all: $(OBJS)
	$(CXX) -o $(DESTDIR)/$(EXEC) $(OBJS) $(LDFLAGS)

clean:
	$(RM) $(OBJS)

cleanall: clean
	$(RM) $(DESTDIR)/$(EXEC)

debug: CXXFLAGS += $(DEBUG)
debug: all

%.o	: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ -c $<

check-syntax:
	g++ $(CXXFLAGS) -fsyntax-only $(CHK_SOURCES)
