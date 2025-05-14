# Paths
OUTPUT_DIR = output
REPORT_OUT_DIR = $(OUTPUT_DIR)/report
FIGURE_SCRIPTS = $(wildcard fig[0-9]*.R)  # List all your figure scripts here
REPORT_SRC = _main.Rmd
REPORT_SECTIONS = $(wildcard [0-9]*.Rmd)

# Date and Git commit hash
VERSION_DATE = $(shell date +%Y%m%d)
GIT_TAG = $(shell git describe --tags --always)
VERSION = $(VERSION_DATE)_$(GIT_TAG)

# Output 
FIGURE_LOGS = $(FIGURE_SCRIPTS:%.R=$(OUTPUT_DIR)/%.pdf)  # Corresponding log files
REPORT_PDF = $(REPORT_OUT_DIR)/main_$(VERSION).pdf
REPORT_DOCX = $(REPORT_OUT_DIR)/main_$(VERSION).docx
REPORT_LATEST_PDF = $(REPORT_OUT_DIR)/main_latest.pdf
REPORT_LATEST_DOCX = $(REPORT_OUT_DIR)/main_latest.docx

all: $(REPORT_PDF)

$(REPORT_OUT_DIR):
	mkdir -p $@

$(FIGURE_LOGS): output/%.pdf: %.R
	Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_yaml = "_output_pdf.yml")'

$(REPORT_PDF): $(REPORT_SRC) $(REPORT_SECTIONS) $(FIGURE_LOGS) | $(REPORT_OUT_DIR)
	Rscript -e "rmarkdown::render('$<', output_file = '$@', output_yaml = '_output_pdf.yml')" 
	cp $@ $(REPORT_LATEST_PDF)

docx: $(REPORT_DOCX)

$(REPORT_DOCX): $(REPORT_SRC) $(REPORT_SECTIONS) $(FIGURE_LOGS) | $(REPORT_OUT_DIR)
	Rscript -e "rmarkdown::render('$<', output_file = '$@', output_yaml = '_output_docx.yml')"
	ln -sf "main_$(VERSION).docx" $(REPORT_LATEST_DOCX)

view:
	open -a Skim $(REPORT_LATEST_PDF)

draft:
	open $(REPORT_LATEST_DOCX)

.PHONY: all figures clean view
