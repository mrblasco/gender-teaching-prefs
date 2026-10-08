# ----------------------------------------------------------------------
# Analysis of Novelty
# ----------------------------------------------------------------------
suppressMessages({
    library(dplyr)
    library(purrr)
    library(tidyr)
    library(knitr)
    library(parallel)
    library(lme4)
    library(broom.mixed)
    library(logger)
})

say <- logger::log_info
warn <- logger::log_warn

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

data_dir    <- file.path("data", "processed")
out_dir     <- ifelse(length(args) >= 1, args[1], tempdir())

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

say("Input dir: {data_dir}")
say("Output dir: {out_dir}")


# ----------------------------------------------------------------------
# Load data
# ----------------------------------------------------------------------

say("Loading data ...")
syllabi_merged <- readRDS(file.path(data_dir, "syllabi_merged.rds"))

say(
    "Loaded {nrow(syllabi_merged)} rows, {ncol(syllabi_merged)} cols."
)


knitr::kable(
    count(syllabi_merged, course_level) |>
    mutate(pc = 100 * n / sum(n)),
    digits = 0
)


# ----------------------------------------------------------------------
# Models
# ----------------------------------------------------------------------

covars <- c("team", "country", "course_level", "prob", "stem", "tot_count")
stopifnot(all(covars %in% names(syllabi_merged)))

vars <- c(covars, "(1|field)", "(1|institution)")

list_models <- list(
    interdisc       = reformulate(vars, "intdisc_rp"),
    women           = reformulate(vars, "female_ratio"),
    conventionality = reformulate(vars, "conventional_rp"),
    atypicality     = reformulate(vars, "atyp_rp"),
    age_readings    = reformulate(vars, "recency_rp")
)

ds_list <- split(
    x = syllabi_merged, 
    f = syllabi_merged$year,
    drop = TRUE
)

for (model in list_models) {
    say("Fitting {deparse(model)}")

    init <- Sys.time()
    fits <- lapply(
        ds_list,
        lme4::lmer,
        formula = model
    )
    elapsed <- Sys.time() - init
    say("Fitted in {round(elapsed)} seconds")

    model_name <- names(list_models)[list_models == model]
    outfile <- file.path(out_dir, paste0(model_name, ".rds"))
    saveRDS(fits, outfile)
    say("Saved to {outfile}")
}