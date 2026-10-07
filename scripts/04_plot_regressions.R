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
    library(patchwork)
    library(ggplot2)
})

source("R/isced.R")
source("R/theme.R")
theme_set(theme_custom())

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

data_dir    <- file.path("data", "processed")
out_dir <- if (length(args) >= 1) args[1] else file.path("output", "montecarlo")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

syllabi_path <- file.path(data_dir, "os_final.rds")
novel_path <- file.path(data_dir, "novel_v2.rds")

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
    stopifnot(all(!is.na(x)), length(x) > 1)
    100 * (rank(x) - 1) / (length(x) - 1)
}

# term_labels <- c(
#     "teamf"     = "Woman (F)",
#     "teamff"    = "Two women (FF)",
#     "teamm"     = "Man (M)",
#     "teammm"    = "Two men (MM)",
#     "teamfm"    = "Mixed-gender (F/M)"
# )

term_labels <- c(
    teamf = "Female alone",
    teamff = "Female + female",
    teammm = "Male + male",
    teamfm = "Mixed"
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

syllabi_merged <- syllabi %>%
    left_join(dplyr::select(novelty, -year), by = "id") %>% 
    left_join(isced_lookup, by = "field") %>%
    mutate(
        recency = year - novelty,
        team = relevel(factor(as.character(team)), ref = "m")
    ) |>
    filter(course_level != "unknown")

log_msg("Merged syllabi: %d rows, %d cols", nrow(syllabi_merged), ncol(syllabi_merged))


# ----------------------------------------------------------------------
# Descriptive statistics
# ----------------------------------------------------------------------

calc_stats <- function(x) {
    x[x == Inf] <- NA
    x <- na.omit(x)
    data.frame(
        mean = mean(x),
        sd = sd(x), 
        med = median(x),
        q1 = quantile(x, 0.25), 
        q3 = quantile(x, 0.75), 
        min = min(x), 
        max = max(x),
        obs = length(x),
        row.names = NULL
    )
}

tbl_desc <- syllabi_merged %>% 
    dplyr::select(novel_med, atyp_med) %>% 
    lapply(calc_stats) %>% 
    bind_rows(.id = "variable")

write.csv(tbl_desc, file.path(out_dir, "descriptives.csv"))

# ----------------------------------------------------------------------
# Plot descriptives
# ----------------------------------------------------------------------

plot_stats <- function(df) {
    df <- df %>%
        mutate(all =  ifelse(grepl("All", term), "All", "Field"))

    df %>%
        ggplot(
            aes(
                x = year,
                y = estimate,
                group = term
            )
        ) +
        geom_line(data = filter(df, all == "Field"), color = "gray", linewidth = 0.1) +
        geom_line(data = filter(df, all == "All"), color = "red", linewidth = 1)
}

novel_stats <- syllabi_merged %>% 
    filter(!is.na(novel_med)) %>%
    bind_rows(., mutate(., field = "All")) %>%
    reframe(
        tidy(lm(novel_med ~ field - 1), conf.int = TRUE),
        .by = year
    )

atyp_stats <- syllabi_merged %>% 
    filter(!is.na(atyp_med), atyp_med != Inf) %>%
    bind_rows(., mutate(., field = "All")) %>%
    reframe(
        tidy(lm(atyp_med ~ field - 1, weights = atyp_n), conf.int = TRUE),
        .by = year
    ) 

recency_stats <- syllabi_merged %>% 
    filter(!is.na(recency)) %>%
    bind_rows(., mutate(., field = "All")) %>%
    reframe(
        tidy(lm(recency ~ field - 1, weights = matched_docs), conf.int = TRUE),
        .by = year
    )

plot_desc_novel <- novel_stats %>% 
    plot_stats() +
    labs(
        x = "Year",
        y = "Teaching conventionality\n(Median Cum. Readings/Citations)"
    )

plot_desc_atyp <-  atyp_stats %>% 
    plot_stats() + 
    scale_y_log10() + 
    labs(
        x = "Year",
        y = "Scientific novelty\n(Atypicality)"
    )

plot_desc_recency <- recency_stats %>% 
    plot_stats() +
    labs(
        x = "Year",
        y = "Mean Readings Age"
    )

p_desc <- plot_desc_novel + plot_desc_atyp + plot_desc_recency
ggsave(
    file.path(out_dir, "01_desc.pdf"),
    device = cairo_pdf,
    width = 7,
    height = 3.8,
    unit = "in"
)


# ----------------------------------------------------------------------
#  Function to fit model by year
# ----------------------------------------------------------------------

covars <- c("team", "country", "course_level", "prob", "stem", "tot_count")

re <- c("(1|field)", "(1|institution)")

list_models <- list(
    interdisc = reformulate(c(covars, re), "rank_percentile(mean_intdisc)"),
    women = reformulate(c(covars, re), "female_ratio"),
    conventionality = reformulate(c(covars, re), "rank_percentile(novel_med)"),
    atypicality = reformulate(c(covars, re), "rank_percentile(atyp_med)"),
    age_readings = reformulate(c(covars, re), "rank_percentile(recency)")
)


fit_by <- function(data, formula, column, fn, ...) {
    data |>
        dplyr::group_by({{ column }}) |>
        dplyr::group_split() |>
        purrr::map(.f = fn, formula = formula, ...)
}



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
#  Interdisciplinarity --- revision
# ----------------------------------------------------------------------
log_msg("Interdisciplinarity...")

fit_intdisc <- syllabi_merged |>
    filter(!is.na(mean_intdisc)) |>
    fit_by(list_models$interdisc, year, lme4::lmer)

# Show results ... 
tail(fit_intdisc) |>
    stargazer::stargazer(type = "text", keep = "team", keep.stat = "n", digits = 2)

coeffs <- fit_intdisc |>
    map(broom::tidy, conf.int = TRUE) |>
    bind_rows(.id = "year") |>
    mutate(year = as.numeric(year))


p_intdisc <- coeffs |>
    filter(grepl("team", term)) |>
    ggplot(aes(year, estimate, color = term, ymin = conf.low, ymax = conf.high)) +
    scale_color_discrete() +
    scale_shape_manual(values = c(1, 16)) + 
    scale_y_continuous() + 
    facet_grid(~term, labeller = labeller(term = term_labels)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(show.legend = FALSE, aes(shape = abs(estimate) > 2 * std.error )) +
    geom_smooth(method = "lm", aes(weight = 1/std.error^2), formula = 'y ~ x') + 
    labs(
        x = "Academic year",
        y = "Interdisciplinarity (%)"
    ) +
    theme(legend.position = "none")


ggsave(
    here::here(out_dir, "04_intdisc.pdf"),
    device = cairo_pdf,
    width = 7.5,
    height = 2.5,
    units = "in"
)

# ----------------------------------------------------------------------
#  Women authors
# ----------------------------------------------------------------------
log_msg("Women authors...")

fit_women <- syllabi_merged |>
    mutate(
        total_authors = female_authors + male_authors,
        female_ratio = (female_authors ) / (male_authors + female_authors),
        .by = c(year)
    ) |>
    filter(total_authors > 0) |>
    fit_by(list_models$women, year, lme4::lmer)

coeffs <- fit_women |>
    map(broom::tidy, conf.int = TRUE) |>
    bind_rows(.id = "year") |>
    mutate(year = as.numeric(year))

p_women <- coeffs |>
    filter(grepl("team", term)) |>
    ggplot(aes(year, estimate, color = term, ymin = conf.low, ymax = conf.high)) +
    scale_color_discrete() +
    scale_shape_manual(values = c(1, 16)) + 
    scale_y_continuous(labels = \(x) 100 * x) + 
    facet_grid(~term, labeller = labeller(term = term_labels)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(show.legend = FALSE, aes(shape = abs(estimate) > 2 * std.error )) +
    geom_smooth(method = "lm", aes(weight = 1/std.error^2), formula = 'y ~ x') + 
    labs(
        x = "Academic year",
        y = "Cited female ratio"
    ) +
    theme(legend.position = "none")

ggsave(
    here::here(out_dir, "05_women.pdf"),
    device = cairo_pdf,
    width = 7.5,
    height = 2.5,
    units = "in"
)

# ----------------------------------------------------------------------
#  Conventionality
# ----------------------------------------------------------------------
log_msg("Conventionality ...")

fits_conventionality <- syllabi_merged |>
    filter(!is.na(novel_med)) |>
    fit_by(list_models$conventionality, year, lme4::lmer)


coeffs <- fits_conventionality |>
    map(broom::tidy, conf.int = TRUE) |>
    bind_rows(.id = "year") |>
    mutate(year = as.numeric(year))

p_conventional <- coeffs |>
    filter(grepl("team", term)) |>
    ggplot(aes(year, estimate, color = term, ymin = conf.low, ymax = conf.high)) +
    scale_color_discrete() +
    scale_shape_manual(values = c(1, 16)) + 
    scale_y_continuous() + 
    facet_grid(~term, labeller = labeller(term = term_labels)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(show.legend = FALSE, aes(shape = abs(estimate) > 2 * std.error )) +
    geom_smooth(method = "lm", aes(weight = 1/std.error^2), formula = 'y ~ x') + 
    labs(
        x = "Academic year",
        y = "Conventionality" |>
            stringr::str_wrap(28)
    ) +
    theme(legend.position = "none")

ggsave(
    here::here(out_dir, "06_conventional.pdf"),
    device = cairo_pdf,
    width = 7.5,
    height = 2.5,
    units = "in"
)

# ----------------------------------------------------------------------
# Atypical
# ----------------------------------------------------------------------
log_msg("Atypicality ...")


fits_atypicality <- syllabi_merged |>
    filter(!is.na(atyp_med)) |>
    fit_by(list_models$atypicality, year, lme4::lmer)


coeffs <- fits_atypicality |>
    map(broom::tidy, conf.int = TRUE) |>
    bind_rows(.id = "year") |>
    mutate(year = as.numeric(year))

p_atypical <- coeffs |>
    filter(grepl("team", term)) |>
    ggplot(aes(year, estimate, color = term, ymin = conf.low, ymax = conf.high)) +
    scale_color_discrete() +
    scale_shape_manual(values = c(1, 16)) + 
    scale_y_continuous() + 
    facet_grid(~term, labeller = labeller(term = term_labels)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(show.legend = FALSE, aes(shape = abs(estimate) > 2 * std.error )) +
    geom_smooth(method = "lm", aes(weight = 1/std.error^2), formula = 'y ~ x') + 
    labs(
        x = "Academic year",
        y = "Atypicality" |>
            stringr::str_wrap(28)
    ) +
    theme(legend.position = "none")

ggsave(
    here::here(out_dir, "07_atypical.pdf"),
    device = cairo_pdf,
    width = 7.5,
    height = 2.5,
    units = "in"
)


# ----------------------------------------------------------------------
# Age of readings
# ----------------------------------------------------------------------
log_msg("Age of readings...")

fits_recency <- syllabi_merged |>
    filter(!is.na(recency)) |>
    fit_by(list_models$age_readings, year, lme4::lmer)


coeffs <- fits_recency |>
    map(broom::tidy, conf.int = TRUE) |>
    bind_rows(.id = "year") |>
    mutate(year = as.numeric(year))

p_recency <- coeffs |>
    filter(grepl("team", term)) |>
    ggplot(aes(year, estimate, color = term, ymin = conf.low, ymax = conf.high)) +
    scale_color_discrete() +
    scale_shape_manual(values = c(1, 16)) + 
    scale_y_continuous(labels = \(x) 100 * x) + 
    facet_grid(~term, labeller = labeller(term = term_labels)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(show.legend = FALSE, aes(shape = abs(estimate) > 2 * std.error )) +
    geom_smooth(method = "lm", aes(weight = 1/std.error^2), formula = 'y ~ x') + 
    labs(
        x = "Academic year",
        y = "Age of readings" |>
            stringr::str_wrap(28)
    ) +
    theme(legend.position = "none")

ggsave(
    here::here(out_dir, "08_age_readings.pdf"),
    device = cairo_pdf,
    width = 7.5,
    height = 2.5,
    units = "in"
)

# ----------------------------------------------------------------------
# Combined
# ----------------------------------------------------------------------

p_combined <- p_conventional + 
    p_intdisc + 
    p_women + 
    p_atypical + 
    p_recency +
    plot_layout(ncol = 1, axes = "collect", axis_titles = "collect")) +
    plot_annotation(tag_levels = "A")


ggsave(
    here::here(out_dir, "09_combined.pdf"),
    device = cairo_pdf,
    width = 7.5,
    height = 7.5,
    units = "in"
)

quit("no")

# OLD 

# ----------------------------------------------------------------------
#  Interdisciplinarity --- Separate regressions by year
# ----------------------------------------------------------------------
interdisc <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(
        interdisc = rank_percentile(mean_intdisc),
        .by = c(year)
    )

model <- interdisc ~ team + country + course_level + scale(prob) + stem +
    (1 | field) + (1 | institution) + scale(log(tot_count))

fit_interdisc <- interdisc %>%
    fit_models_by_year(model)

log_msg(("US only..."))
fit_interdisc_us <- dplyr::filter(interdisc, country == "US") %>% 
    fit_models_by_year(update(model, ~ . - country))

log_msg(("STEM only..."))
fit_interdisc_stem <- dplyr::filter(interdisc, stem) %>%
    fit_models_by_year(update(model, ~ . - stem))


# ----------------------------------------------------------------------
#  Women authors --- Separate regressions by year
# ----------------------------------------------------------------------
women <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(
        total_authors = female_authors + male_authors,
        female_ratio = (female_authors + 1) / (male_authors + female_authors + 2),
        .by = c(year)
    )

model <- female_ratio ~ team + country + course_level + scale(prob) + stem +
    (1 | field) + (1 | institution) + scale(log(tot_count))

fit_women <- women %>%
    fit_models_by_year(model)

log_msg(("US only..."))
fit_women_us <- dplyr::filter(women, country == "US") %>% 
    fit_models_by_year(update(model, ~ . - country))

log_msg(("STEM only..."))
fit_women_stem <- dplyr::filter(women, stem) %>%
    fit_models_by_year(update(model, ~ . - stem))


# ----------------------------------------------------------------------
#  Conventionality --- Separate regressions by year
# ----------------------------------------------------------------------

conventional <- syllabi_merged %>% 
    filter(!is.na(novel_med)) %>%
    mutate(
        conventional = rank_percentile(novel_med),
        .by = c(year)
    )

model <- conventional ~ team + country + course_level + scale(prob) + stem +
    (1 | field) + (1 | institution) + scale(log(tot_count))

fit_conventional <- conventional %>%
    fit_models_by_year(model)

log_msg(("US only..."))
fit_conventional_us <- dplyr::filter(conventional, country == "US") %>% 
    fit_models_by_year(update(model, ~ . - country))

log_msg(("STEM only..."))
fit_conventional_stem <- dplyr::filter(conventional, stem) %>%
    fit_models_by_year(update(model, ~ . - stem))

# ----------------------------------------------------------------------
#  Atypicality --- Separate regressions by year
# ----------------------------------------------------------------------

atypical <- syllabi_merged %>%
    filter(!is.na(atyp_med), atyp_med != Inf) %>%
    mutate(
        atypical = rank_percentile(atyp_med),
        .by = c(year)
    )

model <- atypical ~ team + country + course_level + scale(prob) + 
    scale(recency) + (1 | field) + (1 | institution) + scale(log(atyp_n))

fit_atypical <- atypical %>%
    fit_models_by_year(model)

fit_atypical_us <- dplyr::filter(atypical, country == "US") %>% 
    fit_models_by_year(update(model, ~ . - country))

fit_atypical_stem <- dplyr::filter(atypical, stem) %>%
    fit_models_by_year(model)

# ----------------------------------------------------------------------
#  Age of readings --- Separate regressions by year
# ----------------------------------------------------------------------

age_readings <- syllabi_merged %>%
    filter(!is.na(recency)) %>%
    mutate(
        age_readings = rank_percentile(recency),
        .by = c(year)
    )

model <- age_readings ~ team + country + course_level + scale(prob)  +
    (1 | field) + (1 | institution) + scale(log(matched_docs))

fit_age_readings <- age_readings %>%
    fit_models_by_year(model)

fit_age_readings_us <- dplyr::filter(age_readings, country == "US") %>% 
    fit_models_by_year(update(model, ~ . - country))

fit_age_readings_stem <- dplyr::filter(age_readings, stem) %>%
    fit_models_by_year(model)


# ----------------------------------------------------------------------
# Save
# ----------------------------------------------------------------------

objs <- setdiff(ls(pattern = "fit_"), "fit_models_by_year")
models <- mget(objs)

results <- purrr::map(models, ~ {
    .x %>%
        purrr::map(~ tidy(.x, conf.int = TRUE)) %>%
        dplyr::bind_rows(.id = "year")
})

out <- dplyr::bind_rows(results, .id = "model")
outpath <- file.path(results_dir, "coeffs.rds")
saveRDS(out, outpath)



# coeffs <- fit_conventional %>% lapply(tidy, conf.int = TRUE) %>% bind_rows(.id = "year")

# coeffs %>% 
#     filter(grepl("team", term)) %>%
#     ggplot() +
#     aes(x = as.numeric(year), y = estimate, ymin = conf.low, ymax = conf.high) + 
#     geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#     geom_errorbar(linewidth = 0.25) +
#     geom_point() +
#     geom_smooth(method = "gam", aes(weight = 1 / std.error^2, color = "Smoothed trend (95% CI)"), linewidth = 0.7) +
#     scale_color_brewer(palette = "Set1") + 
#     facet_wrap(~term, labeller = labeller(term = term_labels)) +
#     labs(
#         x = "Year", y = "Estimate"
#     )
