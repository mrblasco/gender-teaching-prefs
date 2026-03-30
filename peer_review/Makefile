
all: docs/2026-03-28-PNASNEXUS-response-to-reviewers.pdf
	open -a Skim $<

docs/%.pdf : PNEXUS/%.Rmd
	Rscript -e "rmarkdown::render('$<', output_file = '../$@')"

