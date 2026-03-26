# ----------------------------------------------------------------------
# Robustness analysis
# ----------------------------------------------------------------------

library(dplyr)
library(lme4)
library(broom.mixed)
library(knitr)
library(parallel)
library(ggplot2)

source("R/paths.R")
source("R/isced.R")
source("R/theme.R")
source("R/utils.R")
source("R/labels.R")

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------
results_dir <- file.path("data", "results")
data_dir <- file.path("data", "processed")

syllabi_path <- file.path(data_dir, "os_final.rds")
novel_path <- file.path(data_dir, "novel_v2.rds")

term_labels <- c(
    "teamf" = "Woman (F)",
    "teamff" = "Two women (FF)",
    "teamm" = "Man (M)",
    "teammm" = "Two men (MM)",
    "teamfm" = "Mixed-gender (F/M)"
)

# ----------------------------------------------------------------------
# Load data
# ----------------------------------------------------------------------
syllabi <- readRDS(syllabi_path)
log_msg("Loaded syllabi: %d rows, %d cols", nrow(syllabi), ncol(syllabi))

novelty <- readRDS(novel_path)
log_msg("Loaded novelty: %d rows, %d cols", nrow(novelty), ncol(novelty))

# ----------------------------------------------------------------------
# Merge data
# ----------------------------------------------------------------------
log_msg("Merging data...")
syllabi_merged <- syllabi %>%
    left_join(dplyr::select(novelty, -year), by = "id") %>% 
    left_join(isced_lookup, by = "field") %>%
    mutate(
        recency = year - novelty,
        team = relevel(factor(as.character(team)), ref = "m")
    )

stopifnot(nrow(syllabi_merged) == nrow(syllabi))



# ================================
# TODO: Data & Variable Checks
# ================================

# TODO: Verify how course levels are harmonized across countries
# TODO: Ensure consistent coding for country, field, course level, and year
# TODO: Validate key variables:
#   - interdisciplinarity
#   - reading recency/novelty
#   - female-author share

# ================================
# TODO: Address Sorting Concerns
# ================================

# TODO: Control for subfield specialization (if possible)
# TODO: Add proxies for course topic (e.g., keywords or classifications)
# TODO: Test whether interdisciplinary courses have more gender-diverse teams
# TODO: Clearly treat results as correlations (not causal)

# ================================
# TODO: Improve Model Specification
# ================================

# TODO: Add richer fixed effects:
#   - country × field × course level × year
# TODO: Compare results with and without these fixed effects
# TODO: Add clustered standard errors (e.g., by institution or field)






# ----------------------------------------------------------------------
#  Function to fit model by year
# ----------------------------------------------------------------------

fit_models_by_year <- function(df, formula, start = 2000, end = 2019, ...) {
    depvar <- all.vars(formula)[1]
    fits <- lapply(start:end, function(j) {
        df_sub <- df[df$year == j, ]
        n_obs <- nrow(df_sub)

        init <- Sys.time()
        fit <- lme4::lmer(formula = formula, data = df_sub, weights = sqrt(matched_docs))
        end <- Sys.time()

        log_msg(
            "fitted %i (in %2.2f secs) | depvar: %s | n_obs: %i",
            j, end - init, depvar, n_obs
        )

        fit
    })
    names(fits) <- start:end
    fits
}

# ----------------------------------------------------------------------
#  Interdisciplinarity --- Separate regressions by year
# ----------------------------------------------------------------------
interdisc <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(
        interdisc = rank_percentile(mean_intdisc),
        .by = c(year)
    )

# baseline model
model <- interdisc ~ team + country + course_level + scale(prob) + stem +
    (1 | field) + (1 | institution) + scale(log(tot_count))

fit_interdisc <- fit_models_by_year(interdisc, model)

# with country x course level interactions
model_full <- interdisc ~ team + scale(prob) + stem + country * course_level +
    (1 | field) + (1 | institution) + scale(log(tot_count))

fit_interdisc_full <- fit_models_by_year(interdisc, model_full)

# with country x course level x field interactions
model_field <- interdisc ~ team + scale(prob) + stem + country + course_level +
    (1 + course_level | field) + (1 | institution) + scale(log(tot_count))

fit_interdisc_field <- fit_models_by_year(interdisc, model_field)

# ---- Show results 

coeffs_base <- fit_interdisc |>
    lapply(broom::tidy, conf.int = TRUE) |> 
    bind_rows(.id = "year")

coeffs_full <- fit_interdisc_full |>
    lapply(broom::tidy, conf.int = TRUE) |> 
    bind_rows(.id = "year")

coeffs <- bind_rows(list("baseline" = coeffs_base, "full" = coeffs_full), .id = "model")

ggplot(
    filter(coeffs, grepl("team", term)),
    aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, color = model) 
) + 
    geom_errorbar() +
    geom_point() +
    facet_wrap(~ term)
