# Directories
OUTPUT_DIR = output
REPORT_DIR = $(OUTPUT_DIR)/report

REPORT_SRC = _main.Rmd
REPORT_SECTIONS = $(wildcard [0-9]*.Rmd)
FIGURE_SCRIPTS = $(wildcard fig[0-9]*.R)

PDF_FORMAT := bookdown::pdf_document2

# Output 
FIGURE_LOGS = $(FIGURE_SCRIPTS:%.R=$(OUTPUT_DIR)/%.pdf)  # Corresponding log files
REPORT_PDF = $(REPORT_DIR)/Gender_Teaching_Syllabi.pdf
REPORT_DOCX = $(REPORT_PDF:.pdf=.docx)

all: pdf

pdf: $(REPORT_PDF)

$(REPORT_DIR):
	mkdir -p $@

$(REPORT_PDF): _main.Rmd $(PDF_CONFIG) $(REPORT_SECTIONS) $(FIGURE_LOGS) $(REPORT_DIR)
	Rscript -e 'rmarkdown::render("$<", "$(PDF_FORMAT)", "$@")'

$(FIGURE_LOGS): output/%.pdf: %.R
	Rscript -e 'rmarkdown::render("$<", "$(PDF_FORMAT)", "$@")'

view:
	open -a Skim $(REPORT_PDF)

clean:
	rm *.fff *.log *.ttt

.PHONY: all clean view

# ---- Word

docx: $(REPORT_DOCX)

$(REPORT_DOCX): $(REPORT_SRC) $(REPORT_SECTIONS) $(FIGURE_LOGS) | $(REPORT_DIR)
	Rscript -e "rmarkdown::render('$<', output_file = '$@', output_yaml = '_output_docx.yml')"
	ln -sf "main_$(VERSION).docx" $(REPORT_LATEST_DOCX)


draft:
	open $(REPORT_LATEST_DOCX)


