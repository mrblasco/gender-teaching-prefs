

responses.pdf : responses.Rmd
	Rscript -e 'rmarkdown::render("$<", output_format = "bookdown::pdf_document2")'

view: 
	open -a Skim responses.pdf