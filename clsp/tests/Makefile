# Copyright 2012 Jan Pokorny <xpokor04@stud.fit.vutbr.cz,
#                             pokorny_jan@seznam.cz>
#
# This file is part of clsp/predator.
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

CLSP = ../clsp
TEST_RUNNER = ./tests.sh

FILES_IN  := $(shell find -name '*.c.raw' | sort)
FILES_REF := $(FILES_IN:%.c.raw=%.debug.ref)

INDENT_DIRECTIVES = -bad -bbb -sob

all : check

files :: ${FILES_REF}

# first preserve comments as they carry control tags
%.c :: %.c.raw | ${CLSP}
	sed -n '/\/\*/bbgn;d;:bgn;p;n;/\*\//bend;bbgn;:end;p' $< > $@.tail
	${CLSP} -E $< | cat - $@.tail | indent ${INDENT_DIRECTIVES} > $@
	rm -f -- $@.tail

# these needs preprocessed source files from raw ones
${FILES_REF} :: %.debug.ref : %.c
	${TEST_RUNNER} $<

check :: ${FILES_REF} ${CLSP}
	${TEST_RUNNER}

force-update: ${FILES_REF}
	touch -m -- ${FILES_IN:%.c.raw=%.c}
	BATCH=1 ${TEST_RUNNER}

${CLSP}:
	make -C $(dir $@) $(notdir $@)

clean:
	${TEST_RUNNER} clean

.PHONY: all files check clean force-update
