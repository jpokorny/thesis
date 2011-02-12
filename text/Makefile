# Master thesis (incl. term project) @ BUT FIT <www.fit.vutbr.cz>
# Symbolic Execution of C Programs Based on Sparse
# Copyright (C) 2011 Jan Pokorný


DOC_SOURCE      = README

.PHONY: html pdf clean clean-doc
#all: 

html: $(DOC_SOURCE).html
$(DOC_SOURCE).html: $(DOC_SOURCE)
	rst2html $< $@

pdf: $(DOC_SOURCE).pdf
$(DOC_SOURCE).pdf: $(DOC_SOURCE).tex
	pdflatex -interaction=batchmode -draftmode $<
	pdflatex -interaction=batchmode $<
	rm -rf -- $(DOC_SOURCE).aux \
              $(DOC_SOURCE).log \
              $(DOC_SOURCE).out \
              $(DOC_SOURCE).tex \
              $(DOC_SOURCE).toc
$(DOC_SOURCE).tex: $(DOC_SOURCE)
	rst2latex $< $@

# Maintenance

clean: clean-doc
	$(MAKE) -C fig $@
clean-doc:
	rm -rf -- $(DOC_SOURCE).html \
              $(DOC_SOURCE).pdf
