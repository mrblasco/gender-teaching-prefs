# ----------------------------------------------------------------------
# Robustness analysis
# ----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
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

theme_set(theme_custom())

results_dir <- file.path("data", "results")
data_dir <- file.path("data", "processed")

# ----- load data --------------------------------------------------
syllabi_merged <- readRDS(file.path(data_dir, "syllabi_merged.rds"))

# ----- params ------
term_labels <- c(
    "teamf" = "Woman (F)",
    "teamff" = "Two women (FF)",
    "teamm" = "Man (M)",
    "teammm" = "Two men (MM)",
    "teamfm" = "Mixed-gender (F/M)"
)

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

extract_coeffs_and_bind <- function(objects) {
    bind_rows(lapply(objects, broom::tidy, conf.int = TRUE), .id = "year")
}

run_analysis <- function(model, data) {
    model_ordered <- update(model, ~ . - team + team_ordered)
    model_course_x_country <- update(model, ~ . + course_level:country)

    res <- list()
    res$baseline <- fit_models_by_year(data, model)
    res$ordered <- fit_models_by_year(data, model_ordered)
    res$course_x_country <- fit_models_by_year(data, model_course_x_country)

    coeffs <- res |>
        lapply(extract_coeffs_and_bind) |> 
        bind_rows(.id = "model")
    coeffs
}

load_or_run_analysis <- function(model, data, filename) {
    if (file.exists(filename)) {
        message("Load data from ", filename)
        return(readRDS(filename))
    } 
    coeffs <- run_analysis(model, interdisc) 
    saveRDS(coeffs, filename)
    return(coeffs)
}

# ----- regressions ------- 

interdisc <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(
        age_readings = rank_percentile(recency),
        atypical = rank_percentile(atyp_med),
        conventional = rank_percentile(novel_med),
        total_authors = female_authors + male_authors,
        female_ratio = (female_authors + 1) / (male_authors + female_authors + 2),
        interdisc = rank_percentile(mean_intdisc),
        .by = c(year)
    )

model <- interdisc ~ team + country + course_level + scale(prob) + stem +
    (1 | field) + (1 | institution) + scale(log(tot_count))


coeffs <- list()

coeffs$interdisc <- load_or_run_analysis(
    model, 
    interdisc, 
    file.path("data", "results", "coeffs_interdisc.rds")
)

coeffs$women <- load_or_run_analysis(
    update(model, female_ratio ~ .), 
    interdisc,
    file.path("data", "results", "coeffs_women.rds")
)

coeffs$conventional <- load_or_run_analysis(
    update(model, conventional ~ .), 
    interdisc,
    file.path("data", "results", "coeffs_conventional.rds")
)

coeffs$atypical <- load_or_run_analysis(
    update(model, atypical ~ .), 
    interdisc,
    file.path("data", "results", "coeffs_atypical.rds")
)

coeffs$age_readings <- load_or_run_analysis(
    update(model, age_readings ~ .), 
    interdisc,
    file.path("data", "results", "coeffs_age_readings.rds")
)

# ----- plots ----- 

depvar_labels <- c(
    "interdisc" = "Interdisciplinarity",
    "women" = "Female authors ratio",
    "conventional" = "Readings conventionality",
    "atypical" = "Scientific novelty",
    "age_readings" = "Age of readings"
)


p <- coeffs |> 
    bind_rows(.id = "depvar") |>
    filter(
        grepl("fm|mf", term), 
        model == "ordered"
    ) |> 
    mutate(
        year = as.numeric(year), 
        base = grepl("fm", term)
    ) |>
    summarise(
        diff = estimate[base] - estimate[!base],
        se_diff = sqrt(sum(std.error^2)),
        .by = c(year, depvar)
    ) |> 
    ggplot(
        aes(
            x = year, 
            y = diff, 
            ymin = diff - 2 * se_diff, 
            ymax = diff + 2 * se_diff,
        ) 
    ) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(color = abs(diff) > 2 * se_diff)) +
    geom_point(aes(color = abs(diff) > 2 * se_diff)) +
    geom_smooth() +
    facet_wrap(
        ~ depvar,
        scales = "free",
        labeller = labeller(depvar = depvar_labels)
    ) +
    scale_y_continuous(name = "Lead gender diff (FM - MF)") +
    scale_color_manual(
        name = "Coeff. diff.",
        values = c("brown", "dodgerblue"),
        labels = c("p < 0.05", "p > 0.05")
    ) + 
    theme(
        legend.position = "bottom",
    )


pdf_filename <- file.path("data", "results", "fig_coeff_diff_order.pdf")
out <- ggsave(pdf_filename, width = 7, height = 5)
system(paste("open", out))

quit()





# ---- order 
model_order <- interdisc ~ team_ordered + country + course_level + scale(prob) + stem +
    (1 | field) + (1 | institution) + scale(log(tot_count))

fit_interdisc_order <- fit_models_by_year(interdisc, model_order)



model_order <- interdisc ~ team_ordered + country + course_level + scale(prob) + stem +
    (1 | field) + (1 | institution) + scale(log(tot_count))

fit_interdisc_order <- fit_models_by_year(interdisc, model_order)

# ----------------------------------------------------------------------
#  Women authors --- Separate regressions by year
# ----------------------------------------------------------------------
women <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(
        team_ordered = relevel(factor(team_ordered), ref = "m"),
        total_authors = female_authors + male_authors,
        female_ratio = (female_authors + 1) / (male_authors + female_authors + 2),
        .by = c(year)
    )

model <- female_ratio ~ team_ordered + country + course_level + scale(prob) + stem +
    (1 | field) + (1 | institution) + scale(log(tot_count))

fit_women <- women %>%
    fit_models_by_year(model)

coeffs <- fit_women |>
    lapply(broom::tidy, conf.int = TRUE) |> 
    bind_rows(.id = "year")

ggplot(
    filter(coeffs, grepl("team", term)),
    aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high) 
) + 
    geom_errorbar() +
    geom_point() +
    facet_wrap(~ term)


# ----------------------------------------------------------------------
#  Conventionality --- Separate regressions by year
# ----------------------------------------------------------------------

conventional <- syllabi_merged %>% 
    filter(!is.na(novel_med)) %>%
    mutate(
        team_ordered = relevel(factor(team_ordered), ref = "m"),
        conventional = rank_percentile(novel_med),
        .by = c(year)
    )

model <- conventional ~ team_ordered + country + course_level + scale(prob) + stem +
    (1 | field) + (1 | institution) + scale(log(tot_count))

fit <- conventional %>%
    fit_models_by_year(model)

coeffs <- fit |>
    lapply(broom::tidy, conf.int = TRUE) |> 
    bind_rows(.id = "year")

ggplot(
    filter(coeffs, grepl("team", term)),
    aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high) 
) + 
    geom_errorbar() +
    geom_point() +
    facet_wrap(~ term)


# ----------------------------------------------------------------------
#  Atypicality --- Separate regressions by year
# ----------------------------------------------------------------------

atypical <- syllabi_merged %>%
    filter(!is.na(atyp_med), atyp_med != Inf) %>%
    mutate(
        team_ordered = relevel(factor(team_ordered), ref = "m"),
        atypical = rank_percentile(atyp_med),
        .by = c(year)
    )

model <- atypical ~ team_ordered + country + course_level + scale(prob) + 
    scale(recency) + (1 | field) + (1 | institution) + scale(log(atyp_n))

fit <- atypical %>%
    fit_models_by_year(model)

coeffs <- fit |>
    lapply(broom::tidy, conf.int = TRUE) |> 
    bind_rows(.id = "year")

ggplot(
    filter(coeffs, grepl("team", term)),
    aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high) 
) + 
    geom_errorbar() +
    geom_point() +
    facet_wrap(~ term)


# ----------------------------------------------------------------------
#  Age of readings --- Separate regressions by year
# ----------------------------------------------------------------------

age_readings <- syllabi_merged %>%
    filter(!is.na(recency)) %>%
    mutate(
        team_ordered = relevel(factor(team_ordered), ref = "m"),
        age_readings = rank_percentile(recency),
        .by = c(year)
    )

model <- age_readings ~ team_ordered + country + course_level + scale(prob)  +
    (1 | field) + (1 | institution) + scale(log(matched_docs))

fit <- age_readings %>%
    fit_models_by_year(model)


coeffs <- fit |>
    lapply(broom::tidy, conf.int = TRUE) |> 
    bind_rows(.id = "year")

ggplot(
    filter(coeffs, grepl("team", term)),
    aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high) 
) + 
    geom_errorbar() +
    geom_point() +
    facet_wrap(~ term)

ggplot(
    filter(coeffs, grepl("fm|mf", term)) |> mutate(year = as.numeric(year)),
    aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, color = term) 
) +    
    geom_errorbar() +
    geom_point() +
    geom_smooth() +
    facet_grid(~term)

coeffs <- fit_women |>
    lapply(broom::tidy, conf.int = TRUE) |> 
    bind_rows(.id = "year")

filter(coeffs, grepl("fm|mf", term)) |> 
    mutate(year = as.numeric(year), base = grepl("fm", term)) |>
    summarise(
        diff = estimate[base] - estimate[!base],
        se_diff = sqrt(sum(std.error^2)),
        .by = year
    ) |> 
    ggplot(
        aes(
            x = year, 
            y = diff, 
            ymin = diff - 2 * se_diff, 
            ymax = diff + 2 * se_diff
        ) 
    ) +
    geom_hline(yintercept = 0) +
    geom_errorbar() +
    geom_point() +
    geom_smooth() 

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

coeffs <- fit_interdisc_order |>
    lapply(broom::tidy, conf.int = TRUE) |> 
    bind_rows(.id = "year")

ggplot(
    filter(coeffs, grepl("team", term)),
    aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high) 
) + 
    geom_errorbar() +
    geom_point() +
    facet_wrap(~ term)
