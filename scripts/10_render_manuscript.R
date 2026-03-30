library(rmarkdown)

input_file <- file.path("manuscript", "_main.Rmd")

timestamp <- format(Sys.time(), "%Y-%m-%d")

out <- render(
  input_file,
  output_format = "bookdown::pdf_document2",
  output_file = "gender_teaching_syllabi.pdf",
  output_dir = file.path("submission", timestamp)
)

message("Rendered ", out)
