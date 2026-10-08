# ======================================================================
# Robustness checks --- full-sample regressions + log transforms
# ----------------------------------------------------------------------
# Produces the robustness figures reported in the SI "Robustness
# Analysis" subsection. For each of the five course-material outcomes we
# fit a single mixed-effects regression on the FULL sample (all years
# pooled, year as a fixed covariate), as a check on the main per-year
# specification. For the recency, conventionality and atypicality
# outcomes we also show a log-transformed version; for the female-author
# ratio we add a quasi-Poisson count model.
#
# Reads the canonical merged dataset (data/processed/syllabi_merged.rds)
# and writes figures to <out_dir>, mirroring the 04/10 plotting scripts.
#
# Outputs (to <out_dir>, default output/05_plot_robustness_checks/):
#   robust_interdisc.pdf      interdisciplinarity (percentile rank)
#   robust_women.pdf          female-author ratio: (A) LMM (B) quasi-Poisson
#   robust_age_readings.pdf   age of readings: (A) rank (B) log
#   robust_conventional.pdf   conventionality: (A) rank (B) log
#   robust_atypical.pdf       atypicality: (A) rank (B) log
#
# Usage:  Rscript scripts/05_plot_robustness_checks.R <out_dir>
# ======================================================================

suppressMessages({
    library(dplyr, warn.conflicts = FALSE)
    library(lme4)
    library(broom.mixed)
    library(ggplot2)
    library(patchwork)
})

source(here::here("R/theme.R"))  # theme_custom()
source(here::here("R/utils.R"))  # rank_percentile(), log_msg()
theme_set(theme_custom())

set.seed(4881)

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) {
    args[1]
} else {
    here::here("output", "05_plot_robustness_checks")
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_dir <- here::here("data", "processed")

term_labels <- c(
    "teamf"  = "Woman (F)",
    "teamff" = "Two women (FF)",
    "teamm"  = "Man (M)",
    "teammm" = "Two men (MM)",
    "teamfm" = "Mixed-gender (F/M)"
)

# ----------------------------------------------------------------------
# Load data (canonical merged dataset; team already releveled to ref "m")
# ----------------------------------------------------------------------
log_msg("Loading data ...")
syllabi_merged <- readRDS(file.path(data_dir, "syllabi_merged.rds"))
log_msg("Loaded %s rows.", format(nrow(syllabi_merged), big.mark = ","))

# ----------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------
# Full-sample mixed model for a percentile-rank (or log) outcome.
fit_full <- function(df, depvar) {
    form <- stats::reformulate(
        c("team", "country", "year", "course_level",
          "scale(log(tot_count))", "scale(prob)", "stem",
          "(1 | field)", "(1 | institution)"),
        response = depvar
    )
    lme4::lmer(form, data = df,
               control = lmerControl(optimizer = "bobyqa",
                                     calc.derivs = FALSE))
}

tidy_team <- function(fit, model = "pr") {
    broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
        filter(grepl("^team", term)) %>%
        mutate(model = model)
}

# Forest panel of team coefficients.
coeff_panel <- function(coeffs, percent = FALSE) {
    p <- ggplot(
        coeffs,
        aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)
    ) +
        geom_vline(aes(linetype = "Men alone (M)", xintercept = 0),
                   color = "red") +
        scale_linetype_manual(values = c("Men alone (M)" = "dashed")) +
        geom_errorbar(width = 0.1) +
        geom_point() +
        scale_y_discrete(labels = term_labels) +
        labs(x = "Estimated coefficient (95% CIs)", y = NULL)
    if (percent) p <- p + scale_x_continuous(labels = scales::percent)
    p
}

save_fig <- function(plot, name, width = 7, height = 4) {
    out <- file.path(out_dir, name)
    ggsave(out, plot, device = cairo_pdf, width = width, height = height)
    log_msg("  wrote %s", name)
}

# ======================================================================
# 1. Interdisciplinarity (percentile rank)
# ======================================================================
interdisc <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(interdisc = rank_percentile(mean_intdisc), .by = year)

p_interdisc <- fit_full(interdisc, "interdisc") %>%
    tidy_team() %>%
    coeff_panel()
save_fig(p_interdisc, "robust_interdisc.pdf", width = 5, height = 3.5)

# ======================================================================
# 2. Female-author ratio: (A) LMM on unadjusted share (B) quasi-Poisson
# ======================================================================
women <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(
        total_authors = female_authors + male_authors,
        # Unadjusted empirical share of women authors (NA when no
        # gender-identified authors); the (f+1)/(f+m+2) adjustment was
        # removed in the third revision (Reviewer #3).
        female_ratio = ifelse(total_authors > 0,
                              female_authors / total_authors, NA_real_),
        .by = year
    )

p_women_lmm <- fit_full(women, "female_ratio") %>%
    tidy_team() %>%
    coeff_panel(percent = TRUE)

fit_qp <- glm(
    female_authors ~ team + year + country + field + offset(log(total_authors)),
    family = quasipoisson, data = women, subset = total_authors > 0
)
p_women_qp <- broom::tidy(fit_qp) %>%
    filter(grepl("^team", term)) %>%
    mutate(conf.low = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error) %>%
    coeff_panel()

p_women <- (p_women_lmm + p_women_qp) + plot_annotation(tag_levels = "A")
save_fig(p_women, "robust_women.pdf", width = 7, height = 4)

# ======================================================================
# 3. Conventionality: (A) percentile rank (B) log
# ======================================================================
conventional <- syllabi_merged %>%
    filter(!is.na(novel_med)) %>%
    mutate(
        conventional = rank_percentile(novel_med),
        conventional_log = log(novel_med),
        .by = year
    )

coeffs_conv <- bind_rows(
    tidy_team(fit_full(conventional, "conventional"), "pr"),
    tidy_team(fit_full(conventional, "conventional_log"), "log")
)
p_conv <- (coeff_panel(filter(coeffs_conv, model == "pr")) +
           coeff_panel(filter(coeffs_conv, model == "log"))) +
    plot_annotation(tag_levels = "A")
save_fig(p_conv, "robust_conventional.pdf", width = 7, height = 4)

# ======================================================================
# 4. Atypicality: (A) percentile rank (B) log
# ======================================================================
atypical <- syllabi_merged %>%
    filter(!is.na(atyp_med), atyp_med != Inf) %>%
    mutate(
        atypical = rank_percentile(atyp_med),
        atypical_log = log(ifelse(atyp_med > 0, atyp_med, 1)),
        .by = year
    )

coeffs_atyp <- bind_rows(
    tidy_team(fit_full(atypical, "atypical"), "pr"),
    tidy_team(fit_full(atypical, "atypical_log"), "log")
)
p_atyp <- (coeff_panel(filter(coeffs_atyp, model == "pr")) +
           coeff_panel(filter(coeffs_atyp, model == "log"))) +
    plot_annotation(tag_levels = "A")
save_fig(p_atyp, "robust_atypical.pdf", width = 7, height = 4)

# ======================================================================
# 5. Age of readings: (A) percentile rank (B) log
# ======================================================================
age_readings <- syllabi_merged %>%
    filter(!is.na(recency)) %>%
    mutate(
        age_readings = rank_percentile(recency),
        age_readings_log = log(ifelse(recency <= 0, 1, recency)),
        .by = year
    )

coeffs_age <- bind_rows(
    tidy_team(fit_full(age_readings, "age_readings"), "pr"),
    tidy_team(fit_full(age_readings, "age_readings_log"), "log")
)
p_age <- (coeff_panel(filter(coeffs_age, model == "pr")) +
          coeff_panel(filter(coeffs_age, model == "log"))) +
    plot_annotation(tag_levels = "A")
save_fig(p_age, "robust_age_readings.pdf", width = 7, height = 4)

log_msg("")
log_msg("Done. Robustness figures written to %s", out_dir)
