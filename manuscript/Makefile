OUTDIR := build
INPUT := main.Rmd 
FILES := $(wildcard sections/*.Rmd) refs.bib _output.yml
PDF := $(OUTDIR)/main.pdf

OPTS := output_dir = "$(OUTDIR)", output_format = "bookdown::pdf_document2"

# ---- Targets 

pdf :  $(OUTDIR) $(PDF)

$(OUTDIR):
	@mkdir -p "$@"

$(PDF): $(INPUT) $(FILES)
	@echo "🚀 Rendering $@..."
	@Rscript -e 'rmarkdown::render("$<", $(OPTS))'
	@echo "Done"

view:
	@open -a Skim $(PDF)





# ----- Canonical version
TIME := $(shell date '+%Y-%m-%d %H:%M:%S')
DAY := $(shell date '+%Y-%m-%d')
OLD := _canonical/second_revision_$(DAY)/_main.tex

canonical: $(OLD)
$(OLD): _build/_main.tex
	@mkdir -p "$(dir $@)"
	cp $< $@

# ---- Diff 
diff: $(PDF:.pdf=_diff.pdf)
	open -a Skim $<

$(PDF:.pdf=_diff.pdf): $(PDF) 
	@latexdiff _canonical/second_revision_2026-09-25/_main.tex $(PDF:.pdf=.tex) > $(@:.pdf=.tex)
	cd $(dir $@) && xelatex _main_diff.tex

# ----- Clean
clean:
	rm -rf $(DIR)
