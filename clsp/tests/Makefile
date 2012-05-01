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
FILES_OUT := $(FILES_IN:%.c.raw=%.c)

all : files check

files :: ${FILES_OUT}

# first preserve comments as they carry control tags
${FILES_OUT} :: %.c : %.c.raw | ${CLSP}
	sed -n '/\/\*/bbgn;d;:bgn;p;n;/\*\//bend;bbgn;:end;p' $< > $@.tail
	${CLSP} -E $< | indent | cat - $@.tail > $@
	rm -f -- $@.tail $(@:.c=.debug.ref)
	${TEST_RUNNER} $@

check :: ${FILES_OUT}
	${TEST_RUNNER}

${CLSP}:
	make -C $(dir $@) $(notdir $@)

clean:
	${TEST_RUNNER} clean

.PHONY: all files check clean
