# ======================================================================
# Heterogeneity + age/cohort confound --- MODEL FITTING ONLY
# ----------------------------------------------------------------------
# This script fits every model behind the "Heterogeneity" and
# "Age and seniority composition" subsections and saves tidy coefficient
# tables. It does NOT draw any figure. Plotting is handled downstream by
# scripts/10_plot_heterogeneity.R, which reads the artefacts written here.
#
# Rationale: fitting the per-year mixed models is slow (~15 min); drawing
# the figures is instant. Keeping them in one script forced a full refit
# every time a figure needed a tweak. Decoupling mirrors the existing
# 03_fit_models.R / 04_make_plots_v2.R split.
#
# Blocks:
#   A. HETEROGENEITY
#      Per-year, level-adjusted mixed-effects regressions for five
#      course-material outcomes, fitted on three samples:
#        * full  -> baseline (all fields, all countries)
#        * STEM   -> STEM fields only
#        * US     -> US institutions only
#      plus a leading-instructor contrast (team_ordered) that splits
#      mixed teams into female-leading (fm) vs male-leading (mf).
#
#   B. AGE / COHORT CONFOUND  (reviewer concern)
#      Task 1  team x course_level interaction (pooled over years).
#      Task 2  per-year, level-adjusted gaps vs the female share.
#
# Outputs (written to <out_dir>, default output/09_fit_heterogeneity/):
#   coeffs_heterogeneity.rds / .csv   full/STEM/US per-year team coeffs
#   coeffs_leading.rds / .csv         leading-instructor per-year coeffs
#   age_t1_coeffs_by_level.rds / .csv team coeffs by course level
#   age_t2_gap_vs_share.rds / .csv    per-year level-adjusted gaps
#   age_composition_by_level.csv      descriptive composition by level
#   age_solo_share_by_level.csv       female solo share by level
#   age_female_share_by_year.csv      female solo share by year
#   fit_summary.txt                   plain-text fitting log
#
# Usage:  Rscript scripts/09_fit_heterogeneity.R <out_dir>
# ======================================================================

suppressMessages({
    library(dplyr, warn.conflicts = FALSE)
    library(tidyr)
    library(lme4)
    library(broom.mixed)
    library(readr)
})

source(here::here("R/utils.R")) # rank_percentile(), log_msg()

set.seed(4881)

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) {
    args[1]
} else {
    here::here("output", "09_fit_heterogeneity")
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_dir <- here::here("data", "processed")

# Plain-text log, mirrored to the console. ------------------------------
summary_lines <- character(0)
say <- function(fmt, ...) {
    line <- sprintf(fmt, ...)
    message(line)
    summary_lines <<- c(summary_lines, line)
}

# ----------------------------------------------------------------------
# Outcome keys. The `model` column in the heterogeneity output follows
# the manuscript's naming scheme (fit_<key>, fit_<key>_stem, fit_<key>_us).
# ----------------------------------------------------------------------
outcome_labels <- c(
    interdisc    = "Interdisciplinarity",
    conventional = "Conventionality",
    age_readings = "Age of readings",
    female_ratio = "Female-authored ratio",
    atypical     = "Atypicality"
)

model_outcome_key <- c(
    interdisc    = "interd",
    conventional = "conv",
    age_readings = "age",
    female_ratio = "women",
    atypical     = "atyp"
)

# Age-confound block uses its own (longer) outcome labels.
age_outcomes <- c(
    interdisc    = "Interdisciplinarity",
    female_ratio = "Female-authored ratio",
    age_readings = "Age of readings (recency)",
    conventional = "Conventionality",
    atypical     = "Scientific novelty (atypicality)"
)

# ----------------------------------------------------------------------
# Load & prepare data (mirrors scripts/07_revision_round3.R)
# ----------------------------------------------------------------------
say("Loading data ...")
syllabi_merged <- readRDS(file.path(data_dir, "syllabi_merged.rds"))
say("Loaded %s rows.", format(nrow(syllabi_merged), big.mark = ","))

# Percentile-rank the outcomes WITHIN year, exactly as the main analysis.
analysis <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(
        age_readings  = rank_percentile(recency),
        atypical      = rank_percentile(atyp_med),
        conventional  = rank_percentile(novel_med),
        interdisc     = rank_percentile(mean_intdisc),
        total_authors = female_authors + male_authors,
        female_ratio  = (female_authors + 1) / (male_authors + female_authors + 2),
        .by = c(year)
    ) %>%
    mutate(
        team         = relevel(factor(as.character(team)), ref = "m"),
        team_ordered = relevel(factor(as.character(team_ordered)), ref = "m"),
        course_level = as.character(course_level),
        course_level = ifelse(course_level == "unknown", NA, course_level),
        course_level = factor(course_level,
                              levels = c("basic", "advanced", "graduate"))
    )

say("Analysis rows (non-missing interdisciplinarity): %s",
    format(nrow(analysis), big.mark = ","))

outcomes <- names(outcome_labels)

# ======================================================================
# BLOCK A --- HETEROGENEITY
# ======================================================================

# Per-year, level-adjusted model. One random intercept per field and
# institution, course-material controls, weighted by sqrt(matched_docs),
# matching the main specification (scripts/07_revision_round3.R).
#
# `country` is only included when the subset spans more than one country
# (it is constant, hence not estimable, in the US-only subset).
base_controls <- c("country", "course_level", "scale(prob)")
base_rand     <- c("(1 | field)", "(1 | institution)", "scale(log(tot_count))")

# Fit `depvar ~ team_term + controls` separately for each year on `df`,
# returning the team coefficients tidied with 95% CIs. Fixed-effect
# factor terms that are constant within a year's data are dropped so the
# model matrix stays estimable (e.g. country in the US-only subset).
fit_by_year <- function(df, depvar, team_term = "team", years = 2000:2019) {
    out <- lapply(years, function(yr) {
        d <- df %>% filter(year == yr, !is.na(.data[[depvar]]))
        if (nrow(d) == 0L || n_distinct(d[[team_term]]) < 2L) {
            return(NULL)
        }
        keep <- base_controls[vapply(base_controls, function(term) {
            v <- all.vars(stats::reformulate(term))[1]
            !v %in% names(d) || n_distinct(d[[v]]) >= 2L
        }, logical(1))]
        form <- stats::reformulate(
            c(team_term, keep, base_rand), response = depvar
        )
        fit <- lme4::lmer(
            form, data = d, weights = sqrt(matched_docs),
            control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
        )
        broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
            filter(grepl("^team", term)) %>%
            mutate(year = as.character(yr))
    })
    bind_rows(out)
}

say("")
say("== Block A: fitting per-year heterogeneity models (full/STEM/US) ==")

coeffs <- list()
for (dv in outcomes) {
    key <- model_outcome_key[[dv]]
    t0 <- Sys.time()

    full <- fit_by_year(analysis, dv) %>%
        mutate(model = sprintf("fit_%s", key))
    stem <- fit_by_year(filter(analysis, stem), dv) %>%
        mutate(model = sprintf("fit_%s_stem", key))
    us <- fit_by_year(filter(analysis, country == "US"), dv) %>%
        mutate(model = sprintf("fit_%s_us", key))

    coeffs[[dv]] <- bind_rows(full, stem, us) %>%
        mutate(depvar = dv, outcome = outcome_labels[[dv]])

    say("   fitted %-13s full/STEM/US  (%.1f min)",
        dv, as.numeric(difftime(Sys.time(), t0, units = "mins")))
}
coeffs <- bind_rows(coeffs)

saveRDS(coeffs, file.path(out_dir, "coeffs_heterogeneity.rds"))
write_csv(coeffs, file.path(out_dir, "coeffs_heterogeneity.csv"))

# Leading-instructor contrast (team_ordered splits mixed into fm/mf) ----
say("")
say("== Block A: fitting leading-instructor (ordered) models ==")

coeffs_leading <- list()
for (dv in outcomes) {
    t0 <- Sys.time()
    coeffs_leading[[dv]] <- fit_by_year(analysis, dv,
                                        team_term = "team_ordered") %>%
        mutate(depvar = dv, outcome = outcome_labels[[dv]])
    say("   fitted %-13s ordered  (%.1f min)",
        dv, as.numeric(difftime(Sys.time(), t0, units = "mins")))
}
coeffs_leading <- bind_rows(coeffs_leading)

saveRDS(coeffs_leading, file.path(out_dir, "coeffs_leading.rds"))
write_csv(coeffs_leading, file.path(out_dir, "coeffs_leading.csv"))

# ======================================================================
# BLOCK B --- AGE / COHORT CONFOUND
# ======================================================================

# ---------------------------------------------------------------------
# Task 1 descriptive: composition / female solo share by course level
# ---------------------------------------------------------------------
comp_by_level <- analysis %>%
    filter(!is.na(course_level)) %>%
    count(course_level, team) %>%
    group_by(course_level) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup()
write_csv(comp_by_level, file.path(out_dir, "age_composition_by_level.csv"))

solo_share <- analysis %>%
    filter(team %in% c("m", "f"), !is.na(course_level)) %>%
    group_by(course_level) %>%
    summarise(female_share = 100 * mean(team == "f"), n = n(), .groups = "drop")
write_csv(solo_share, file.path(out_dir, "age_solo_share_by_level.csv"))

say("")
say("== Age confound, Task 1: female share among solo courses, by level ==")
for (i in seq_len(nrow(solo_share))) {
    say("   %-9s  female solo share = %4.1f%%  (n = %s)",
        as.character(solo_share$course_level[i]),
        solo_share$female_share[i],
        format(solo_share$n[i], big.mark = ","))
}

# ---------------------------------------------------------------------
# Task 1: team x course_level interaction (pooled over years)
# ---------------------------------------------------------------------
age_base_rhs <- paste(
    "country + scale(prob) + stem +",
    "(1 | field) + (1 | institution) + (1 | year) + scale(log(tot_count))"
)

fit_level <- function(depvar, interact) {
    rhs_team <- if (interact) "team * course_level" else "team + course_level"
    form <- as.formula(sprintf("%s ~ %s + %s", depvar, rhs_team, age_base_rhs))
    df <- analysis %>% filter(!is.na(course_level), !is.na(.data[[depvar]]))
    t0 <- Sys.time()
    fit <- lme4::lmer(form, data = df, weights = sqrt(matched_docs),
                      control = lmerControl(optimizer = "bobyqa",
                                            calc.derivs = FALSE))
    say("   fitted %-13s interact=%-5s  n=%s  (%.1f min)",
        depvar, interact, format(nrow(df), big.mark = ","),
        as.numeric(difftime(Sys.time(), t0, units = "mins")))
    fit
}

# Recover team contrasts WITHIN each course level from the interaction
# model via hand-built linear combinations of fixef + vcov.
team_within_level <- function(fit) {
    fe <- fixef(fit)
    V  <- as.matrix(vcov(fit))
    nm <- names(fe)
    teams   <- c("teamf", "teamff", "teamfm", "teammm")
    levels3 <- c("basic", "advanced", "graduate")
    lvl_terms <- c(advanced = "course_leveladvanced",
                   graduate = "course_levelgraduate")
    res <- list()
    for (lv in levels3) {
        for (tm in teams) {
            v <- setNames(rep(0, length(fe)), nm)
            if (tm %in% nm) v[tm] <- 1
            if (lv != "basic") {
                ix <- paste0(tm, ":", lvl_terms[[lv]])
                if (ix %in% nm) v[ix] <- 1
            }
            est <- sum(v * fe)
            se  <- sqrt(as.numeric(t(v) %*% V %*% v))
            res[[length(res) + 1]] <- data.frame(
                course_level = lv, term = tm,
                estimate = est, std.error = se,
                conf.low = est - 1.96 * se, conf.high = est + 1.96 * se
            )
        }
    }
    bind_rows(res)
}

team_pooled <- function(fit) {
    broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
        filter(grepl("^team", term), !grepl(":", term)) %>%
        transmute(course_level = "pooled", term,
                  estimate, std.error, conf.low, conf.high)
}

say("")
say("== Age confound, Task 1: fitting pooled and interaction models ==")
t1_list <- list()
for (dv in names(age_outcomes)) {
    fit_pool <- fit_level(dv, interact = FALSE)
    fit_int  <- fit_level(dv, interact = TRUE)
    t1_list[[dv]] <- bind_rows(
        team_pooled(fit_pool),
        team_within_level(fit_int)
    ) %>% mutate(depvar = dv, outcome = age_outcomes[[dv]])
}
t1_coeffs <- bind_rows(t1_list)

saveRDS(t1_coeffs, file.path(out_dir, "age_t1_coeffs_by_level.rds"))
write_csv(t1_coeffs, file.path(out_dir, "age_t1_coeffs_by_level.csv"))

# ---------------------------------------------------------------------
# Task 2: per-year, level-adjusted gaps + female share by year
# ---------------------------------------------------------------------
female_share_by_year <- analysis %>%
    filter(team %in% c("m", "f")) %>%
    group_by(year) %>%
    summarise(female_share = 100 * mean(team == "f"), .groups = "drop")
write_csv(female_share_by_year, file.path(out_dir, "age_female_share_by_year.csv"))

say("")
say("== Age confound, Task 2: female solo share by year ==")
say("   %d: %.1f%%   ->   %d: %.1f%%",
    min(female_share_by_year$year),
    female_share_by_year$female_share[which.min(female_share_by_year$year)],
    max(female_share_by_year$year),
    female_share_by_year$female_share[which.max(female_share_by_year$year)])

per_year_gap <- function(depvar) {
    form <- as.formula(sprintf(
        "%s ~ team + course_level + country + scale(prob) + stem + (1 | field) + (1 | institution) + scale(log(tot_count))",
        depvar))
    out <- lapply(2000:2019, function(yr) {
        df <- analysis %>% filter(year == yr, !is.na(course_level),
                                  !is.na(.data[[depvar]]))
        fit <- lme4::lmer(form, data = df, weights = sqrt(matched_docs),
                          control = lmerControl(optimizer = "bobyqa",
                                                calc.derivs = FALSE))
        broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
            filter(term %in% c("teamf", "teamfm")) %>%
            transmute(year = yr, term, estimate, std.error,
                      conf.low, conf.high)
    })
    say("   done per-year gaps for %s", depvar)
    bind_rows(out)
}

say("")
say("== Age confound, Task 2: fitting per-year, level-adjusted models ==")
t2_gaps <- bind_rows(lapply(names(age_outcomes), function(dv) {
    per_year_gap(dv) %>% mutate(depvar = dv, outcome = age_outcomes[[dv]])
}))
t2_gaps <- t2_gaps %>% left_join(female_share_by_year, by = "year")

saveRDS(t2_gaps, file.path(out_dir, "age_t2_gap_vs_share.rds"))
write_csv(t2_gaps, file.path(out_dir, "age_t2_gap_vs_share.csv"))

# ----------------------------------------------------------------------
# Write plain-text fitting log
# ----------------------------------------------------------------------
writeLines(summary_lines, file.path(out_dir, "fit_summary.txt"))

message("")
message("Done fitting. Coefficient tables written to ", out_dir)
message("Draw figures with: scripts/10_plot_heterogeneity.R")
