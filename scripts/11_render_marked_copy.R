
branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
branch <- gsub("/", "-", branch)

tex_new <- file.path(branch, "_main.tex")
tex_old <- file.path("revision-v2", "_main.tex")
tex_diff <- "diff.tex"

setwd("submission")

system(sprintf("latexdiff %s %s > %s", tex_old, tex_new, tex_diff))
system(sprintf("pdflatex %s", tex_diff))

