library(rmarkdown)


input_file <- file.path("manuscript", "_main.Rmd")

out <- render(
    input_file,
    output_format = "bookdown::pdf_document2"
)

message("Rendered ", out)