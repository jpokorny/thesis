# Copyright (C) 2012 Jan Pokorny <pokorny_jan@seznam.cz>
#
# This file is part of predator.
#
# predator is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# predator is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with predator.  If not, see <http://www.gnu.org/licenses/>.

CC         = c99
CFLAGS     = -Wall -Wextra -pedantic
CFLAGS    += -pipe
LDFLAGS    = -Wl,--as-needed
LDFLAGS   += -ldl  # dlopen,...

RELEASE    = 0
# available: cl, fwnull, sl, fa or <empty>
ANALYZER   = 

ifeq (1,${RELEASE})
  CFLAGS  += -O2 -march=native -mtune=native
  LDFLAGS += -Wl,-O1
else
  CFLAGS  += -g -ggdb3
endif

ifneq (,${ANALYZER})
  CFLAGS += -DHAS_CL
else
  CFLAGS += -rdynamic  # required for symbol resolving "back against us"
endif

# sparse

SPARSE_DIR      = ../sparse-llvm
SPARSE_LIB      = sparse
SPARSE_LIB_PATH = ${SPARSE_DIR}/lib${SPARSE_LIB}.a

LDFLAGS        += -L${SPARSE_DIR} -l${SPARSE_LIB}
INCLUDES       += ${SPARSE_DIR}

# Code Listener + analyzers
PREDATOR_DIR    =  ../predator_cl
INCLUDES       +=  ${PREDATOR_DIR}/include/cl ${PREDATOR_DIR}/cl

# do not warn about C99 and similar issues in included headers
CFLAGS         += $(foreach dir,${INCLUDES},-isystem $(dir))
#CFLAGS         +=  -Wno-system-headers

# local code

PROGRAM  = clsp
OBJECTS  = clsp.o   \
  clsp_options.o    \
  clsp_proceed.o    \
  clsp_types.o      \
  clsp_enum_color.o \
  clsp_enum_ec.o \
  type_enumerator.o \
  hash_table.o      \
  clsp_gccplugstub.o

TEST_PROGRAMS += test_swapfd


all:: ${PROGRAM}

${SPARSE_LIB_PATH}:
	$(MAKE) -C ${SPARSE_DIR} $(notdir $@)

define ANALYZER_template =
  $(1)_dir := $${PREDATOR_DIR}/$(1)
  $(1)_path := $${PREDATOR_DIR}/$(1)_build/lib$(1).so
  $(1):: $(1)_path
  $(1)_path:
	$(MAKE) -C $${$(1)_dir} CMAKE="cmake -D GCC_HOST='$${GCC_HOST}'"
  PHONIES        += $(1) $(1)_dir $(1)_path
  ANALYZER_CLEAN += $${$(1)_dir}
  LDFLAGS        += -L$${PREDATOR_DIR}/$(1)_build -l$(1)
endef
ifneq (,${ANALYZER})
  GCC_HOST := $(shell PATH=/usr/bin:$PATH which gcc)
  $(eval $(call ANALYZER_template,${ANALYZER}))
endif

${OBJECTS}:: %.o : %.c
	$(CC) $(CFLAGS) -o $@ -c $<

${PROGRAM}:: ${OBJECTS} ${ANALYZER} ${SPARSE_LIB_PATH}
	g++ $(CFLAGS) -o $@ ${OBJECTS} $(LDFLAGS)

${TEST_PROGRAMS}:: % : %.c
	$(CC) $(CFLAGS) -o $@ $<

check:: $(PROGRAM)
	./tests.sh

clean: clean-check
	$(RM) -f -- $(OBJECTS) ${TEST_PROGRAMS} core vgcore.*

clean-sparse:
	$(MAKE) -C $(SPARSE_DIR) clean

clean-analyzer:
	for a in $(ANALYZER_CLEAN); do $(MAKE) -C $$a clean; done

clean-check-prev:
	./tests.sh clean_prev

clean-check:
	./tests.sh clean

mrproper: clean clean-sparse clean-analyzer

.PHONY: all check clean ${PHONIES} \
        clean-sparse clean-analyzer clean-check-prev clean-check mrproper
