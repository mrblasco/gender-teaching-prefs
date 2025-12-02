OUTDIR = docs

all: $(OUTDIR) pdf docx


pdf: $(OUTDIR)/responses.pdf
html: $(OUTDIR)/responses.html
docx: $(OUTDIR)/responses.docx

$(OUTDIR): 
	mkdir -p $@

$(OUTDIR)/%.html : %.Rmd
	@Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format = "distill::distill_article")'
	@open $@

$(OUTDIR)/%.pdf : %.Rmd
	@Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format = "bookdown::pdf_document2")'
	@open -a Skim $@

$(OUTDIR)/%.docx : %.Rmd
	@Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format = "bookdown::word_document2")'
	@open $@
