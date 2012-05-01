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
