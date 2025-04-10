# Paths
OUTPUT_DIR = output
REPORT_OUT_DIR = $(OUTPUT_DIR)/report
FIGURE_SCRIPTS = $(wildcard *.R)  # List all your figure scripts here
FIGURE_LOGS = $(FIGURE_SCRIPTS:%.R=figures/%.log)  # Corresponding log files
REPORT_SRC = _main.Rmd
REPORT_SECTIONS = $(wildcard [0-9]*.Rmd)

# Date and Git commit hash
VERSION_DATE = $(shell date +%Y%m%d)
GIT_TAG = $(shell git describe --tags --always)
VERSION = $(VERSION_DATE)_$(GIT_TAG)

# Version
REPORT_OUT_FILE = $(REPORT_OUT_DIR)/main_$(VERSION).pdf
REPORT_LATEST = $(REPORT_OUT_DIR)/main_latest.pdf

# Default target
all: $(REPORT_OUT_FILE)

$(OUTPUT_DIR): 
	mkdir -p $@/report

# Generate figures
$(FIGURE_LOGS): figures/%.log: %.R
	Rscript $< > $@

# Render report
$(REPORT_OUT_FILE): $(REPORT_SRC) $(REPORT_SECTIONS) $(FIGURE_LOGS) | $(OUTPUT_DIR)
	Rscript -e "rmarkdown::render('$<', output_file = '$@')"
	ln -sf main_$(VERSION).pdf $(REPORT_LATEST)


# Clean figures and output
clean:
	rm -f figures/*.pdf figures/*.log
	rm -f $(REPORT_OUT_DIR)/main_*.pdf
	rm -f $(REPORT_LATEST)

view:
	open -a Skim $(REPORT_LATEST)

.PHONY: all figures clean view
