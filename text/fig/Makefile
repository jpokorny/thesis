# Master thesis (incl. term project) @ BUT FIT <www.fit.vutbr.cz>
# Symbolic Execution of C Programs Based on Sparse
# Copyright (C) 2011 Jan Pokorný

# Figures with original in "dia" format
DIA_CONV        =  dia
DIA_CONV_USE    ?= eps-pango
# DIA_CONV_USE    ?= eps
# DIA_CONV_USE    ?= eps-builtin
EPS_CONV        =  epstopdf
EPS_CONV_OPTS   ?= 
FIGS_DIA        := $(wildcard *.dia)

# Figures with original in "svg" format
SVG_CONV        =  inkscape
SVG_CONV_OPTS   ?= --export-area-drawing
FIGS_SVG        := $(wildcard *.svg)

# Figures with original in other (non-convertible) formats
FIGS_PNG        := $(wildcard *.png) req/assignment.png
FIGS_PDF        := $(wildcard *.pdf) req/vut-zp2.pdf req/fit-zp2.pdf

# Grouped
FIGS_PHONY      =  $(FIGS_DIA:.dia=) \
                   $(FIGS_SVG:.svg=) \
                   $(FIGS_PNG:.png=)
FIGS_PRODUCTS   =  $(FIGS_DIA:.dia=.pdf) \
                   $(FIGS_SVG:.svg=.pdf)


.PHONY: all clean $(FIGS_PHONY)
all: $(FIGS_PHONY)

# For "dia" format
$(FIGS_DIA:.dia=): %: %.pdf
$(FIGS_DIA:.dia=.pdf): %.pdf: %.dia
	$(DIA_CONV) -t $(DIA_CONV_USE) -e /dev/stdout $^ \
        | head -n-1                                  \
        | $(EPS_CONV) $(EPS_CONV_OPTS) -f -o $@

# For "svg" format
$(FIGS_SVG:.svg=): %: %.pdf
$(FIGS_SVG:.svg=.pdf): %.pdf: %.svg
	$(SVG_CONV) -A $@ $(SVG_CONV_OPTS) $^

# For other (non-convertible) formats
$(FIGS_PNG:.png=): %: %.png
$(FIGS_PDF:.pdf=): %: %.pdf


# Maintenance
clean:
	rm -rf -- $(FIGS_PRODUCTS)
