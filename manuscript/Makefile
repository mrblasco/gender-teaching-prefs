DIR := _build
LOGDIR := logs

PDF := $(DIR)/_main.pdf
FILES := $(wildcard *.Rmd) refs.bib _output.yml

TIME := $(shell date '+%Y-%m-%d %H:%M:%S')
DAY := $(shell date '+%Y-%m-%d')
OLD := _canonical/second_revision_$(DAY)/_main.tex


pdf : $(PDF)
	open -a Skim $<

$(PDF): _main.Rmd $(FILES)
	@mkdir -p "$(dir $@)" "$(LOGDIR)"
	@echo "[$(TIME)] - Rendering $@ ..." >> "$(LOGDIR)/render.log"
	@Rscript -e 'rmarkdown::render("$<", output_dir = "$(dir $@)", output_format = "bookdown::pdf_document2")' \
		>> "$(LOGDIR)/render.log" 2>&1

# ----- Canonical version
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
