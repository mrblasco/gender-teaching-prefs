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
})

say <- logger::log_info


# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

data_dir    <- file.path("data", "processed")
out_dir <- if (length(args) >= 1) args[1] else file.path("output", "montecarlo")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
say("Output dir: {out_dir}")


# ----------------------------------------------------------------------
# Utils
# ----------------------------------------------------------------------
log_msg <- function(fmt, ...) {
    logger::log_info(sprintf(fmt = fmt, ...))
}

center <- function(x) {
    as.numeric(scale(x, scale = FALSE, center = TRUE))
}

rank_percentile <- function(x) {
    stopifnot(length(x) > 1)
    if (anyNA(x)) warning("x contains NA values")
    100 * (rank(x, na.last = "keep") - 1) / (sum(!is.na(x)) - 1)
}


term_labels <- c(
    teamf = "Female alone",
    teamff = "Female + female",
    teammm = "Male + male",
    teamfm = "Mixed"
)


# ----------------------------------------------------------------------
# Load data
# ----------------------------------------------------------------------

say("Loading data ...")
syllabi_merged <- readRDS(file.path(data_dir, "syllabi_merged.rds"))

say("Loaded {format(nrow(syllabi_merged), big.mark = ',')} rows")
dplyr::glimpse(syllabi_merged)


syllabi_merged <- syllabi_merged |>
    group_by(year) |>
    mutate(
        total_authors = female_authors + male_authors,
        female_ratio = ifelse(
            total_authors > 0,
            female_authors / total_authors,
            NA_real_
        )
    )

# ----------------------------------------------------------------------
# Models
# ----------------------------------------------------------------------

covars <- c("team", "country", "course_level", "prob", "stem", "tot_count")
stopifnot(all(covars %in% names(syllabi_merged)))

vars <- c(covars, "(1|field)", "(1|institution)")

list_models <- list(
    interdisc = reformulate(vars, "rank_percentile(mean_intdisc)"),
    women = reformulate(vars, "female_ratio"),
    conventionality = reformulate(vars, "rank_percentile(novel_med)"),
    atypicality = reformulate(vars, "rank_percentile(atyp_med)"),
    age_readings = reformulate(vars, "rank_percentile(recency)")
)

ds_list <- syllabi_merged |>
    split(syllabi_merged$year)

for (model in list_models) {
    say("Fitting {deparse(model)}")
    print(dim(ds_list))

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