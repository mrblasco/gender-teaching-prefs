library(rmarkdown)

input_file <- file.path("manuscript", "_main.Rmd")

timestamp <- format(Sys.time(), "%Y-%m-%d")
branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
branch <- gsub("/", "-", branch)

out <- render(
  input_file,
  output_format = "bookdown::pdf_document2",
  output_dir = file.path("submission", branch)
)

message("Rendered to ", out)
system(paste("open", out))