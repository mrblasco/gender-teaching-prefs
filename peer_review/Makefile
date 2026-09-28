FILES := \
	PNEXUS/04_third_revision/response.pdf

all: $(FILES)
	open -a Skim $<

PNEXUS/04_third_revision/response.pdf : PNEXUS/04_third_revision/response.Rmd
	Rscript -e 'rmarkdown::render("$<")'