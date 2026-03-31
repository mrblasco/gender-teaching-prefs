input <- file.path("peer_review", "PNEXUS", "2026-03-28-response-to-reviewers.Rmd")

out <- rmarkdown::render(
    input,
    output_dir = file.path("submission", "peer_review")
)

message("Success saved in ", out)
system(paste("open", out))
