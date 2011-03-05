# Master thesis (incl. term project) @ BUT FIT <www.fit.vutbr.cz>
# Symbolic Execution of C Programs Based on Sparse
# Copyright (C) 2011 Jan Pokorný

DOC_NAME           =  sp
DOC_MAIN_SRC       =  main.tex
DOC_PRELUDE_SRCS   =  prelude-arrangement.tex     \
                      prelude-metadata.tex        \
                      prelude-packages.tex        \
                      prelude-customization.tex

# Files that can change between builds
OTHERS             =  fitthesis.cls               \
                      csplainnat.bst              \
                      ref/references.bib

FIGS               =  req/vut-zp2                 \
                      req/fit-zp2                 \
                      req/assignment              \
                      schema_chodu                \
                      cl-block-diagram
FIGS_DIR           =  fig

INPUTFILES_GETTER  =  generate_inputfiles.tex
INPUTFILES_PREREQ  =  prelude-arrangement.tex
INPUTFILES_LISTS   =  inputfiles-chapters inputfiles-appendices

METADATA_GETTER    =  generate_metadata.tex
METADATA_PREREQ    =  prelude-metadata.tex
METADATA_FILES     =  metadata-abstract-cs metadata-abstract-en \
                      metadata-keywords-cs metadata-keywords-en

PATCH_DRAFT        =  setdraft.patch
PATCH_FINAL        =  setfinal.patch
PATCH_TARGET       =  prelude-arrangement.tex

BUILD_DONE_FILE    =  build.done

BACKUP_DIR         =  bck

SPELL_TEXT         = spell-text
SPELL_TEXT_REF     = spell-text-ref
SPELL_METADATA     = spell-metadata
SPELL_METADATA_REF = spell-metadata-ref

###

#BATCHMODE          =  1
XELATEX            =  xelatex
ifdef BATCHMODE
    XELATEXFLAGS   =  -interaction=batchmode
else
    XELATEXFLAGS   =  -interaction=errorstopmode
endif

BIBER              =  biber

CZECHLANG          =  1
ifdef CZECHLANG
    VLNA           =  vlna -v KkSsVvZzOoUuAaIi
else
    VLNA           =  true
endif

PURGE_CMD = echo "purging..."; \
            for f in $$(ls -1 *.te~ 2>/dev/null); do \
                mv -f "$$f" "`echo $$f | sed "s/te~/tex/"`"; \
            done; \
            rm -f -- \
                     $(METADATA_FILES)   \
                     $(INPUTFILES_LISTS) \
                     $(DOC_NAME).aux     \
                     $(DOC_NAME).log     \
                     $(DOC_NAME).out     \
                     $(DOC_NAME).toc     \
                     $(DOC_NAME).bcf     \
                     $(DOC_NAME).blg     \
                     $(DOC_NAME).run.xml \
                     $(DOC_NAME).bbl     \
                     $(DOC_NAME).idx

#                     $(DOC_NAME).brf

###

.PHONY: all figures purge clean deepclean \
        inputfiles metadata               \
        setdraft setfinal                 \
        statistics hunspell hunspell-metadata hunspell-text \
        backup
all: $(DOC_NAME).pdf

# Tho-phased prerequisities to take changes in figures into account
$(BUILD_DONE_FILE): figures $(OTHERS) $(INPUTFILES_LISTS)
	touch $@

inputfiles: $(INPUTFILES_LISTS)
$(INPUTFILES_LISTS): $(INPUTFILES_LISTS:=.tex)
	@for i in $^; do mv $$i $$(echo $$i | sed "s/.tex//g"); done
$(INPUTFILES_LISTS:=.tex): $(INPUTFILES_GETTER) $(INPUTFILES_PREREQ)
	@pdflatex -interaction=batchmode -draftmode $<; \
	rm -rf -- $(<:.tex=.aux) $(<:.tex=.log)

metadata: $(METADATA_FILES)
$(METADATA_FILES): $(METADATA_FILES:=.tex)
	@for i in $^; do                                                           \
        cat $$i | sed "s/\\\\enc@update/§/g" |                                 \
            sed -e "s/{\\\\protect[^§]\+[§]/{/g" -e "s/\\\\par\s\+/\\n\\n/g" | \
            detex | fmt -u > $$(echo $$i | sed "s/.tex//g");                   \
        rm -f -- $$i;                                                          \
    done
$(METADATA_FILES:=.tex): $(METADATA_GETTER) $(METADATA_PREREQ)
	pdflatex -interaction=batchmode -draftmode $<
	rm -rf -- $(<:.tex=.aux) $(<:.tex=.log)

$(DOC_NAME).pdf: $(DOC_MAIN_SRC) $(DOC_PRELUDE_SRC) $(BUILD_DONE_FILE)
	$(VLNA) $< $$(cat $(INPUTFILES_LISTS) | sed "s/$$/.tex/")
	$(XELATEX) $(XELATEXFLAGS) -jobname $(@:.pdf=) $<      
	$(BIBER) $(@:.pdf=)                                    
	$(XELATEX) $(XELATEXFLAGS) -jobname $(@:.pdf=) $<      
#	$(XELATEX) $(XELATEXFLAGS) -jobname $(@:.pdf=) $<
	@$(PURGE_CMD)

figures:
	$(MAKE) -C $(FIGS_DIR) $(FIGS)

statistics: $(INPUTFILES_LISTS)
	@for i in $^; do                                         \
        texcount $$(cat $$i | sed "s/$$/.tex/"); echo "---"; \
    done

hunspell: hunspell-text hunspell-metadata

hunspell-text: $(SPELL_TEXT)
$(SPELL_TEXT): $(INPUTFILES_LISTS)
	@:> $@
	@for i in $^; do for j in $$(cat $$i | sed "s/$$/.tex/"); do \
        echo "---" >> $@; echo $$j >> $@; echo "---" >> $@;      \
        cat $$j | hunspell -l -t -d cs_CZ,en_US -p spell.dic |   \
                  sort | uniq >> $@;                             \
    done; done
	[ -f $(SPELL_TEXT_REF) ] && diff $(SPELL_TEXT_REF) $@ ||:

hunspell-metadata: $(SPELL_METADATA)
$(SPELL_METADATA): $(METADATA_PREREQ)
	@cat $^ | hunspell -l -t -d cs_CZ,en_US -p spell.dic | \
              sort | uniq > $@
	[ -f $(SPELL_METADATA_REF) ] && diff $(SPELL_METADATA_REF) $@ ||:

purge:
	@$(PURGE_CMD)

clean:
	@$(PURGE_CMD)
	rm -f -- $(DOC_NAME).pdf

deepclean: clean
	$(MAKE) -C $<

setdraft:
	cat $(PATCH_DRAFT) | patch -F3 -N -r - $(PATCH_TARGET) || :

setfinal:
	cat $(PATCH_FINAL) | patch -F3 -N -r - $(PATCH_TARGET) || :

backup: $(INPUTFILES_LISTS) | $(BACKUP_DIR)
	tar cjpf $(BACKUP_DIR)/$$(date +"%y%m%d_%H%M%S").tar.bz      \
             $(DOC_MAIN_SRC)                                     \
             $(DOC_PRELUDE_SRC)                                  \
             $(OTHERS)                                           \
             $(patsubst %,$(FIGS_DIR)/%.*,$(FIGS))               \
             $(shell cat $(INPUTFILES_LISTS) | sed "s/$$/.tex/") \
             Makefile
	rm -rf -- $(INPUTFILES_LISTS)

$(BACKUP_DIR):
	mkdir $@
