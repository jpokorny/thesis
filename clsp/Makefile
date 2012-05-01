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
CFLAGS    += -Wall -Wextra -Wwrite-strings -pedantic #-errors
#CFLAGS    += -Werror
CFLAGS    += -pipe
LDFLAGS   += -Wl,--as-needed
LDFLAGS   += -ldl  # dlopen, ...
DBGCFLAGS  = -O0 -gdwarf-4 -g3 -fno-inline

RELEASE    = 0
USE_LTO    = 1

ANALYZERS  = cl_test fwnull sl fa
# one from above or <empty>
ANALYZER   =
CL_OPTS   :=

ifeq (1,${RELEASE})
  CFLAGS  += -O2 -march=native -mtune=native
  CFLAGS  += -Wno-format  # or -DC99STRICT, both to suppress "%n$" warnings
  LDFLAGS += -Wl,-O1
else
  CFLAGS  += ${DBGCFLAGS}
  CFLAGS  += -Wno-format
  CFLAGS  += -Wswitch-enum
  SP_OPTS  = "CFLAGS += ${DBGCFLAGS}"
  CL_OPTS += CL_DEBUG=1
endif
ifneq (,${ANALYZER})
  CFLAGS  += -DHAS_CL
else
  CFLAGS  += -rdynamic  # required for symbol resolving "back against us"
endif
ifeq (1,${USE_LTO})
    LTOFLAGS = -flto
    CFLAGS += ${LTOFLAGS}
    LDFLAGS += -fuse-linker-plugin ${LTOFLAGS}
endif

# this test and dependency usage borrowed from sparse's Makefile
HAVE_GCC_DEP:=$(shell touch .gcc-test.c                                   \
                && $(CC) -c -MD,.gcc-test.d .gcc-test.c &>/dev/null       \
                && echo 'yes'; rm -f .gcc-test.d .gcc-test.o .gcc-test.c)
ifeq ($(HAVE_GCC_DEP),yes)
  OBJECTS_CFLAGS  += -Wp,-MD,$(@D)/.$(@F).d
  DEP_FILES := $(wildcard .*.o.d)
endif

GETLDFLAGS = -L$(dir $(1)) -l$(basename $(patsubst lib%,%,$(notdir $(1))))

# sparse (using static library, shared is also possible)

SPARSE_DIR       = ../sparse-llvm
INCLUDE_DIRS    += ${SPARSE_DIR}
SPARSE_LIB_NAME  = sparse
SPARSE_LIB      := ${SPARSE_DIR}/lib${SPARSE_LIB_NAME}.a
LDFLAGS         += $(call GETLDFLAGS,${SPARSE_LIB})

# Code Listener + analyzers

PREDATOR_DIR     = ../predator
INCLUDE_DIRS    += ${PREDATOR_DIR}/include/cl ${PREDATOR_DIR}/cl
CL_OPTS         += GCC_HOST='$(shell PATH=/usr/bin:$PATH which gcc)'
CL_OPTS         := $(foreach cl_opt,${CL_OPTS},-D ${cl_opt})

# do not warn about C99 and similar issues in included headers
CFLAGS          += $(foreach dir,${INCLUDE_DIRS},-isystem ${dir})
#CFLAGS          +=  -Wno-system-headers

# local code

PROGRAM  = clsp
OBJECTS  = clsp.o    \
  clsp-ret.o         \
  clsp-color.o       \
  clsp-out-base.o    \
  clsp-out-ext.o     \
  clsp-options.o     \
  clsp-interact.o    \
  clsp-use-cl.o      \
  clsp-use-sparse.o  \
  clsp-conv.o        \
  clsp-types.o       \
  clsp-emit.o        \
  type_enumerator.o  \
  hash_table.o       \
  clsp-gccplugstub.o

TEST_PROGRAMS   += test_swapfd
COMPLETION       = completion/${PROGRAM}


all :: ${PROGRAM}


${SPARSE_LIB}:
	$(if {$SP_OPTS},$(shell echo "${SP_OPTS}" > ${SPARSE_DIR}/local.mk))
	$(MAKE) -C ${SPARSE_DIR} V=1 $(@F)
	$(RM) ${SPARSE_DIR}/local.mk

define ANALYZER_template =
  # following patsubst does: cl_test -> cl
  $(1)_dir :=  $$(patsubst %_test,%,$${PREDATOR_DIR}/$(1))
  $(1)_path := $${$(1)_dir}_build/lib$(1).so
  $(1) :: $${$(1)_path}
  $${$(1)_path}:
	$(MAKE) -C $${$(1)_dir} CMAKE="cmake $${CL_OPTS}"
  PHONIES        += $(1)
  ANALYZER_CLEAN += $${$(1)_dir}
  ifeq (${ANALYZER},$(1))
    LDFLAGS        += $$(call GETLDFLAGS,$${$(1)_path})
  endif
endef
$(foreach analyzer,${ANALYZERS},$(eval $(call ANALYZER_template,${analyzer})))

$(if ${DEP_FILES},$(eval include ${DEP_FILES}))

${OBJECTS} : %.o : %.c
	$(CC) $(CFLAGS) $(OBJECTS_CFLAGS) -o $@ -c $<

${PROGRAM} :: ${OBJECTS} ${SPARSE_LIB} ${ANALYZER}
	g++ $(CFLAGS) -o $@ ${OBJECTS} $(LDFLAGS)

${TEST_PROGRAMS} :: % : %.c
	$(CC) $(CFLAGS) -o $@ $<

completion :: ${COMPLETION}

${COMPLETION} :: ${PROGRAM}
	./${PROGRAM} --bash > $@

check :: $(PROGRAM)
	tests/tests.sh

clean : clean-check
	$(RM) -f -- $(OBJECTS) ${TEST_PROGRAMS} ${DEP_FILES} core vgcore.*

clean-sparse:
	$(MAKE) -C $(SPARSE_DIR) clean

clean-analyzer:
	for a in $(ANALYZER_CLEAN); do $(MAKE) -C $$a clean; done

clean-check-prev:
	./tests.sh clean_prev

clean-check:
	./tests.sh clean

mrproper: clean clean-sparse clean-analyzer

.PHONY: all completion check clean ${PHONIES} \
        clean-sparse clean-analyzer clean-check-prev clean-check mrproper
