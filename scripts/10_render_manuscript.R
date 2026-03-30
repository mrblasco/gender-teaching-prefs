library(rmarkdown)
library(bookdown)

input_file <- file.path("manuscript", "_main.Rmd")

output_dir <- file.path("manuscript", "build")
version_tag <- format(Sys.Date(), "%Y-%m-%d")
tex_file <- file.path(output_dir, paste0("manuscript_", version_tag, ".tex"))
pdf_file <- file.path(output_dir, paste0("manuscript_", version_tag, ".pdf"))

out <- render(
    input = input_file,
    output_format = "bookdown::pdf_document2",
    output_file = basename(pdf_file),
    output_dir = output_dir,
    clean = FALSE,
    envir = new.env()
)

generated_tex <- sub(".pdf$", ".tex", out)
file.rename(generated_tex, tex_file)

message("Rendered PDF: ", out)
message("Saved TeX: ", tex_file)
