OUTDIR = docs

all: $(OUTDIR) $(OUTDIR)/responses.html

$(OUTDIR): 
	mkdir -p $@

$(OUTDIR)/%.html : %.Rmd
	@Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format = "distill::distill_article")'
	@open $@