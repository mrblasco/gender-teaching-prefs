OUTDIR = docs

all: $(OUTDIR) html pdf

pdf: $(OUTDIR)/responses.pdf
html: $(OUTDIR)/responses.html

$(OUTDIR): 
	mkdir -p $@

$(OUTDIR)/%.html : %.Rmd
	@Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format = "distill::distill_article")'
	@open $@

$(OUTDIR)/%.pdf : %.Rmd
	@Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format = "bookdown::pdf_document2")'
