#
# This file is distributed under the GNU General Public License. See
# COPYING for details.
#

SUBDIRS = server utils

# This should be use only during the development process
.DEFAULT_GOAL = debug

# The makefile rule
RULE =

CXXFLAGS += -02

.PHONY: all $(SUBDIRS) server clean cleanall debug

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@ $(RULE)

# If a 'rule' depend on 'another rule' use the following syntax
# rule : another-rule
server: utils

clean: RULE = clean
clean: all

cleanall: RULE = cleanall
cleanall: all

debug: RULE = debug
debug: CXXFLAGS = ""
debug: all
