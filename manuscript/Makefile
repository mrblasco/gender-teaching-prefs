
DIR := _build
LOGDIR := logs
PDF := $(DIR)/_main.pdf
FILES := $(wildcard *.Rmd) refs.bib _output.yml

pdf : $(PDF)
	open -a Skim $<

$(PDF): _main.Rmd $(FILES)
	@echo "Rendering $@ ..."
	@mkdir -p "$(dir $@)" "$(LOGDIR)"
	@Rscript -e 'rmarkdown::render("$<", output_dir = "$(dir $@)", output_format = "bookdown::pdf_document2")' \
		> $(LOGDIR)/render.log 2>&1

clean: 
	rm -rf $(DIR)